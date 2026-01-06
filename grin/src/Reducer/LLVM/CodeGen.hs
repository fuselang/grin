{-# LANGUAGE LambdaCase, TupleSections, DataKinds, RecursiveDo, RecordWildCards, OverloadedStrings, CPP #-}

module Reducer.LLVM.CodeGen
  ( codeGen
  , codeGenWithGC
  , toLLVM
  ) where

import Text.Printf
import Control.Monad as M
import Control.Monad.State
import Data.Functor.Foldable as Foldable
import Lens.Micro.Platform

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.ByteString.Short as ShortByteString
import Data.String (fromString)
import Text.Printf (printf)
import Lens.Micro.Mtl

import LLVM.AST hiding (callingConvention, functionAttributes)
import LLVM.AST.AddrSpace
import LLVM.AST.Type as LLVM
import qualified LLVM.AST.Typed as LLVM
import LLVM.AST.Constant as C hiding (Add, ICmp)
import LLVM.AST.IntegerPredicate
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FunctionAttribute as FA
import qualified LLVM.AST.RMWOperation as RMWOperation
import LLVM.AST.Global as Global

#ifdef WITH_LLVM_HS
import LLVM.Context
import LLVM.Module
#else
-- LLVM.Pretty is not available for LLVM 15, would need llvm-hs-pretty
-- import LLVM.Pretty (ppllvm)
#endif

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BSShort

import Grin.Grin as Grin
import Grin.Pretty
import Grin.TypeEnv hiding (Type, typeOfVal)
import qualified Grin.TypeEnv as TypeEnv
import Reducer.LLVM.Base
import Reducer.LLVM.PrimOps
import Reducer.LLVM.TypeGen
import Reducer.LLVM.InferType



debugMode :: Bool
debugMode = True

toLLVM :: String -> AST.Module -> IO BS.ByteString
#ifdef WITH_LLVM_HS
toLLVM fname mod = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
  BS.writeFile fname llvm
  pure llvm
#else
toLLVM fname mod = do
  -- LLVM.Pretty (ppllvm) is not available for LLVM 15
  -- Fallback: write a placeholder message
  let llvm = BS.pack $ "; LLVM IR output requires WITH_LLVM_HS flag for LLVM 15\n; Module: " ++ show (AST.moduleName mod) ++ "\n"
  BS.writeFile fname llvm
  pure llvm
#endif

codeGenLit :: Lit -> CG C.Constant
codeGenLit = \case
  LInt64 v  -> pure $ Int {integerBits=64, integerValue=fromIntegral v}
  LWord64 v -> pure $ Int {integerBits=64, integerValue=fromIntegral v}
  LFloat v  -> pure $ C.Float {floatValue=F.Single v}
  LBool v   -> pure $ Int {integerBits=1, integerValue=if v then 1 else 0}
  LChar v   -> pure $ Int {integerBits=8, integerValue=fromIntegral $ fromEnum v}
  LString v -> C.GlobalReference <$> strName v  -- LLVM 15: GlobalReference only takes name

strName :: Text.Text -> CG AST.Name
strName str = do
  mName <- use $ envStringMap . at str
  case mName of
    Just n -> pure n
    Nothing -> do
      counter <- envStringCounter <<%= succ
      let n = Name $ fromString $ "str." ++ show counter
      envStringMap %= Map.insert str n
      pure n

codeGenVal :: Val -> CG Operand
codeGenVal val = case val of
  -- TODO: var tag node support
  ConstTagNode tag args -> do
    opArgs <- mapM codeGenVal args

    valT <- typeOfVal val
    let T_NodeSet ns = valT

    ty <- typeOfVal val
    let cgTy = toCGType ty
        TaggedUnion{..} = cgTaggedUnion cgTy
        nodeName = packName $ printf "node_%s" (show $ PP tag)
        -- set node items
        build agg (item, TUIndex{..}) = do
          codeGenLocalVar nodeName (cgLLVMType cgTy) $ AST.InsertValue
            { aggregate = agg
            , element   = item
            , indices'  = [1 + tuStructIndex, tuArrayIndex]
            , metadata  = []
            }

    -- set tag
    tagId <- getTagId tag
    let agg0 = ConstantOperand $ C.Struct
          { structName    = Nothing
          , isPacked      = True
          , memberValues  = tagId : [C.Undef t | t <- tail $ elementTypes tuLLVMType]
          }
    foldM build agg0 $ zip opArgs $ V.toList $ Map.findWithDefault undefined tag tuMapping

  ValTag tag  -> ConstantOperand <$> getTagId tag
  Unit        -> pure unit
  Lit lit     -> ConstantOperand <$> codeGenLit lit
  Var name    -> do
                  Map.lookup name <$> gets _constantMap >>= \case
                    -- QUESTION: what is this?
                    Nothing -> do
                      ty <- getVarType name
                      pure $ LocalReference (cgLLVMType ty) (mkNameG name)
                    Just operand  -> pure operand

  Undefined t -> pure . ConstantOperand . Undef . cgLLVMType. toCGType $ t

  _ -> error $ printf "codeGenVal: %s" (show $ pretty val)

getCPatConstant :: CPat -> CG Constant
getCPatConstant = \case
  TagPat  tag       -> getTagId tag
  LitPat  lit       -> codeGenLit lit
  NodePat tag args  -> getTagId tag
  DefaultPat        -> pure C.TokenNone

getCPatName :: CPat -> Grin.Name
getCPatName = \case
  TagPat  tag   -> tagName tag
  LitPat  lit   -> case lit of
    LInt64 v  -> "int_" <> showTS v
    LWord64 v -> "word_" <> showTS v
    LBool v   -> "bool_" <> showTS v
    LChar v   -> "char_" <> showTS v
    LString v -> error "pattern match on string is not supported"
    LFloat v  -> error "pattern match on float is not supported"
    other     -> error $ "pattern match not implemented: " ++ show other
  NodePat tag _ -> tagName tag
  DefaultPat  -> "default"
 where
  tagName (Tag c name) = showTS c <> name

-- https://stackoverflow.com/questions/6374355/llvm-assembly-assign-integer-constant-to-register
{-
  NOTE: if the cata result is a monad then it means the codegen is sequential

  IDEA: write an untyped codegen ; which does not rely on HPT calculated types
-}

toModule :: Env -> AST.Module
toModule Env{..} = defaultModule
  { moduleName = "basic"
  , moduleDefinitions = heapPointerDefs ++ stringDefinitions ++ reverse _envDefinitions
  }
  where
    -- Only include heap pointer for bump allocator mode
    heapPointerDefs = case _envGCMode of
      GC_BumpAllocator -> [heapPointerDef]
      GC_Boehm         -> []

    heapPointerDef = GlobalDefinition globalVariableDefaults
      { name          = mkName (heapPointerName)
      , Global.type'  = i64
      , initializer   = Just $ Int 64 0
      }

    stringDefinitions = concat
      [ [ GlobalDefinition globalVariableDefaults
            { name = valAstName
            , Global.type' = arrayTy
            , initializer = Just $ C.Array i8 $ [Int 8 $ fromIntegral $ fromEnum v0 | v0 <- stringVal]
            }
        , GlobalDefinition globalVariableDefaults
            { name = astName
            , Global.type' = stringStructType
            , initializer = Just $ C.Struct Nothing False -- TODO: Set struct name
                [ C.GetElementPtr
                    { inBounds = True
                    , type' = arrayTy  -- LLVM 15: GEP requires explicit element type
                    , address = GlobalReference valAstName  -- LLVM 15: GlobalReference only takes name
                    , indices = [Int {integerBits=64, integerValue=0}, Int {integerBits=64, integerValue=0}]
                    }
                , Int 64 $ fromIntegral $ length stringVal
                ]
            }
        ]
      | (stringVal0, astName@(Name astNameBS)) <- Map.toList _envStringMap
      , let stringVal = Text.unpack stringVal0
      , let valAstName = Name $ BSShort.pack $ (BSShort.unpack astNameBS) ++ (BSShort.unpack ".val") -- Append ShortByteStrings
      , let arrayTy = ArrayType (fromIntegral (length stringVal)) i8
      ]

{-
  type of:
    ok - SApp        Name [SimpleVal]
    ok - SReturn     Val
    ?? - SStore      Val
    ?? - SFetchI     Name (Maybe Int) -- fetch a full node or a single node item in low level GRIN
    ok - SUpdate     Name Val
-}

-- | Generate LLVM code with default GC mode (bump allocator)
codeGen :: TypeEnv -> Exp -> AST.Module
codeGen = codeGenWithGC GC_BumpAllocator

-- | Generate LLVM code with specified GC mode
codeGenWithGC :: GCMode -> TypeEnv -> Exp -> AST.Module
codeGenWithGC gcMode typeEnv exp = toModule $ flip execState (emptyEnv {_envTypeEnv = typeEnv, _envGCMode = gcMode}) $ para folder exp where
  folder :: ExpF (Exp, CG Result) -> CG Result
  folder = \case
    SReturnF val -> do
      ty <- typeOfVal val
      O (toCGType ty) <$> codeGenVal val
    SBlockF a -> snd $ a

    EBindF (leftExp, leftResultM) lpat (_,rightResultM) -> do
      leftResult <- case (leftExp, lpat) of
        -- FIXME: this is an ugly hack to compile SStore ; because it requires the binder name to for type lookup
        (SStore val, Var name) -> do
          varT <- getVarType name
          nodeLocation <- codeGenIncreaseHeapPointer varT
          codeGenStoreNode val nodeLocation -- TODO
          pure $ O locationCGType nodeLocation

        -- normal case ; this should be the only case here normally
        _ -> leftResultM
      case lpat of
          VarTagNode{} -> error $ printf "TODO: codegen not implemented %s" (show $ pretty lpat)
          ConstTagNode tag args -> do
            (cgTy,operand) <- getOperand ("node_" <> showTS (PP tag)) leftResult
            let mapping = tuMapping $ cgTaggedUnion cgTy
            -- bind node pattern variables
            forM_ (zip (V.toList $ Map.findWithDefault undefined tag mapping) args) $ \(TUIndex{..}, arg) -> case arg of
              Var argName -> do
                let indices = [1 + tuStructIndex, tuArrayIndex]
                emit [(mkNameG argName) := AST.ExtractValue {aggregate = operand, indices' = indices, metadata = []}]
              _ -> pure ()
          Var name -> do
            getOperand name leftResult >>= addConstant name . snd
          _ -> getOperand "tmp" leftResult >> pure ()
      rightResultM

    SAppF name args -> do
      (retType, argTypes) <- getFunctionType name
      operands <- mapM codeGenVal args
      operandsTypes <- mapM (fmap toCGType . typeOfVal) args
      -- convert values to function argument type
      convertedArgs <- sequence $ zipWith3 codeGenValueConversion operandsTypes operands argTypes
      let findExternalName :: TypeEnv.Name -> Maybe External
          findExternalName n = List.find ((n ==) . eName) (externals exp)
      case findExternalName name of
        Just e -> codeExternal e convertedArgs
        Nothing -> do
          -- call to top level functions
          let functionType      = FunctionType
                { resultType    = cgLLVMType retType
                , argumentTypes = map cgLLVMType argTypes
                , isVarArg      = False
                }
          pure . I retType $ AST.Call
            { tailCallKind        = Just Tail
            , callingConvention   = CC.Fast
            , returnAttributes    = []
            , type'               = functionType  -- LLVM 15: requires explicit function type
            , function            = Right . ConstantOperand $ GlobalReference (mkNameG name)  -- LLVM 15: GlobalReference only takes name
            , arguments           = zip convertedArgs (repeat [])
            , functionAttributes  = []
            , metadata            = []
            }

    AltF _ a -> snd a

    ECaseF val alts -> typeOfVal val >>= \case -- distinct implementation for tagged unions and simple types
      T_SimpleType{} -> do
        opVal <- codeGenVal val
        codeGenCase opVal alts $ \_ -> pure ()

      T_NodeSet nodeSet -> do
        tuVal <- codeGenVal val
        tagVal <- codeGenExtractTag tuVal
        let valTU = taggedUnion nodeSet
        codeGenCase tagVal alts $ \case
          NodePat tag args -> case Map.lookup tag $ tuMapping valTU of
            Nothing -> pure ()
            Just mapping -> do
            --let mapping = Map.findWithDefault undefined tag $ tuMapping valTU
              -- bind cpat variables
              forM_ (zip args $ V.toList mapping) $ \(argName, TUIndex{..}) -> do
                let indices = [1 + tuStructIndex, tuArrayIndex]
                emit [(mkNameG argName) := AST.ExtractValue {aggregate = tuVal, indices' = indices, metadata = []}]
          DefaultPat -> pure ()
          _ -> error "not implemented"

    DefF name args (_,body) -> do
      -- clear def local state
      let clearDefState = modify' $ \env -> env
            { _envBasicBlocks       = mempty
            , _envInstructions      = mempty
            , _constantMap          = mempty
            , _currentBlockName     = mkName ""
            , _envBlockInstructions = mempty
            , _envBlockOrder        = mempty
            }
      clearDefState
      activeBlock (mkNameG $ name <> ".entry")
      (cgTy, result) <- body >>= getOperand ("result." <> name)
      (cgRetType, argTypes) <- getFunctionType name
      let llvmReturnType = cgLLVMType cgRetType

      returnValue <- codeGenValueConversion cgTy result cgRetType

      closeBlock $ Ret
        { returnOperand = if llvmReturnType == VoidType then Nothing else Just returnValue
        , metadata'     = []
        }

      when debugMode $ do
        errorBlock
      blockInstructions <- Map.delete (mkName "") <$> gets _envBlockInstructions
      unless (Map.null blockInstructions) $ error $ printf "unclosed blocks in %s\n  %s" name (show blockInstructions)
      blocks <- gets _envBasicBlocks
      let def = GlobalDefinition functionDefaults
            { name        = mkNameG name
            , parameters  = ([Parameter (cgLLVMType argType) (mkNameG a) [] | (a, argType) <- zip args argTypes], False) -- HINT: False - no var args
            , returnType  = llvmReturnType
            , basicBlocks = Map.elems blocks
            , callingConvention = if name == "grinMain" then CC.C else CC.Fast
            , linkage = if name == "grinMain" then L.External else L.Private
            , functionAttributes = [Right $ FA.StringAttribute "no-jump-tables" "true"]
            }
      clearDefState
      modify' (\env@Env{..} -> env {_envDefinitions = def : _envDefinitions})
      pure $ O unitCGType unit

    ProgramF exts defs -> do
      -- register prim fun lib
      runtimeErrorExternal
      -- Register GC_malloc external when using Boehm GC
      gcMode <- gets _envGCMode
      when (gcMode == GC_Boehm) gcMallocExternal
      mapM registerPrimFunLib exts
      sequence_ (map snd defs) >> pure (O unitCGType unit)

    SFetchIF name Nothing -> do
      -- load tag
      tagAddress <- codeGenVal $ Var name
      tagVal <- codeGenLocalVar "tag" tagLLVMType $ Load
        { volatile        = False
        , type'           = tagLLVMType  -- LLVM 15: Load requires explicit type
        , address         = tagAddress
        , maybeAtomicity  = Nothing
        , alignment       = 1
        , metadata        = []
        }
      -- switch on possible tags
      TypeEnv{..} <- gets _envTypeEnv
      let locs          = case Map.lookup name _variable of
            Just (T_SimpleType (T_Location l)) -> l
            Just (T_SimpleType T_UnspecifiedLocation) -> []
            x -> error $ printf "variable %s can not be fetched, %s is not a location type" name (show $ pretty x)
          nodeSet       = mconcat [_location V.! loc | loc <- locs]
          resultCGType  = toCGType $ T_NodeSet nodeSet
          resultTU      = cgTaggedUnion resultCGType
      codeGenTagSwitch tagVal nodeSet $ \tag items -> do
        let nodeCGType  = toCGType $ T_NodeSet $ Map.singleton tag items
            nodeTU      = cgTaggedUnion nodeCGType
        nodeAddress <- codeGenBitCast ("ptr_" <> showTS (PP tag)) tagAddress ptr  -- LLVM 15: opaque pointers
        nodeVal <- codeGenLocalVar ("node_" <> showTS (PP tag)) (cgLLVMType nodeCGType) $ Load
          { volatile        = False
          , type'           = cgLLVMType nodeCGType  -- LLVM 15: Load requires explicit type
          , address         = nodeAddress
          , maybeAtomicity  = Nothing
          , alignment       = 1
          , metadata        = []
          }
        (resultCGType,) <$> copyTaggedUnion nodeVal nodeTU resultTU

    SUpdateF name val -> do
      nodeLocation <- codeGenVal $ Var name
      codeGenStoreNode val nodeLocation
      pure $ O unitCGType unit

    SStoreF val -> do
      valTy <- typeOfVal val
      nodeLocation <- codeGenIncreaseHeapPointer $ toCGType valTy
      codeGenStoreNode val nodeLocation
      pure $ O locationCGType nodeLocation

    expF -> error $ printf "missing codegen for:\n%s" (show $ pretty $ embed $ fmap fst expF)

codeGenStoreNode :: Val -> Operand -> CG ()
codeGenStoreNode val nodeLocation = do
  tuVal <- codeGenVal val
  tagVal <- codeGenExtractTag tuVal
  valT <- typeOfVal val
  let T_NodeSet nodeSet = valT

  let valueTU = taggedUnion nodeSet
  codeGenTagSwitch tagVal nodeSet $ \tag items -> do
    let nodeTU = taggedUnion $ Map.singleton tag items
    nodeVal <- copyTaggedUnion tuVal valueTU nodeTU
    nodeAddress <- codeGenBitCast ("ptr_" <> showTS (PP tag)) nodeLocation ptr  -- LLVM 15: opaque pointers
    emit [Do Store
      { volatile        = False
      , address         = nodeAddress
      , value           = nodeVal
      , maybeAtomicity  = Nothing
      , alignment       = 1
      , metadata        = []
      }]
    pure $ (unitCGType, unit)
  pure ()

convertStringOperand t o = case (cgType t,o) of
  (T_SimpleType T_String, ConstantOperand stringRef@(GlobalReference{}))
    -> ConstantOperand $ C.GetElementPtr
        { inBounds = False
        , type' = stringStructType  -- LLVM 15: GEP requires explicit element type
        , address = stringRef
        , indices = [Int {integerBits=64, integerValue=0}, Int {integerBits=64, integerValue=0}]
        }
  _ -> o

codeGenCase :: Operand -> [(Alt, CG Result)] -> (CPat -> CG ()) -> CG Result
codeGenCase opVal alts bindingGen = do
  curBlockName <- gets _currentBlockName

  let isDefault = \case
        (Alt DefaultPat _, _) -> True
        _ -> False
      (defaultAlts, normalAlts) = List.partition isDefault alts
  when (length defaultAlts > 1) $ error "multiple default patterns"
  let orderedAlts = defaultAlts ++ normalAlts

  (altDests, altValues, altCGTypes) <- fmap List.unzip3 . forM orderedAlts $ \(Alt cpat _, altBody) -> do
    altCPatVal <- getCPatConstant cpat
    altEntryBlock <- uniqueName ("block." <> getCPatName cpat)
    activeBlock altEntryBlock

    bindingGen cpat

    altResult <- altBody
    (altCGTy, altOp) <- getOperand ("result." <> getCPatName cpat) altResult

    lastAltBlock <- gets _currentBlockName

    pure ((altCPatVal, altEntryBlock), (altOp, lastAltBlock, altCGTy), altCGTy)

  let resultCGType = commonCGType altCGTypes
  switchExit <- uniqueName "block.exit" -- this is the next block

  altConvertedValues <- forM altValues $ \(altOp, lastAltBlock, altCGTy) -> do
    activeBlock lastAltBlock
    -- HINT: convert alt result to common type
    convertedAltOp <- codeGenValueConversion altCGTy altOp resultCGType
    closeBlock $ Br
      { dest      = switchExit
      , metadata' = []
      }
    pure (convertedAltOp, lastAltBlock)

  activeBlock curBlockName
  let (defaultDest, normalAltDests) = if null defaultAlts
        then (if debugMode then mkName "error_block" else switchExit, altDests)
        else (snd $ head altDests, tail altDests)
  closeBlock $ Switch
        { operand0'   = opVal
        , defaultDest = defaultDest -- QUESTION: do we want to catch this error?
        , dests       = normalAltDests
        , metadata'   = []
        }

  activeBlock switchExit

  pure . I resultCGType $ Phi
    { type'           = cgLLVMType resultCGType
    , incomingValues  = altConvertedValues ++ if debugMode then [] else [(undef (cgLLVMType resultCGType), curBlockName)]
    , metadata        = []
    }

-- merge heap pointers from alt branches
codeGenTagSwitch :: Operand -> NodeSet -> (Tag -> Vector SimpleType -> CG (CGType, Operand)) -> CG Result
codeGenTagSwitch tagVal nodeSet tagAltGen | Map.size nodeSet > 1 = do
  let possibleNodes = Map.toList nodeSet
  curBlockName <- gets _currentBlockName

  (altDests, altValues, altCGTypes) <- fmap List.unzip3 . forM possibleNodes $ \(tag, items) -> do
    let cpat = TagPat tag
    altEntryBlock <- uniqueName ("block." <> getCPatName cpat)
    altCPatVal <- getCPatConstant cpat
    activeBlock altEntryBlock

    (altCGTy, altOp) <- tagAltGen tag items

    lastAltBlock <- gets _currentBlockName

    pure ((altCPatVal, altEntryBlock), (altOp, lastAltBlock, altCGTy), altCGTy)

  let resultCGType = commonCGType altCGTypes
  switchExit <- uniqueName "block.exit" -- this is the next block

  altConvertedValues <- forM altValues $ \(altOp, lastAltBlock, altCGTy) -> do
    activeBlock lastAltBlock
    -- HINT: convert alt result to common type
    convertedAltOp <- codeGenValueConversion altCGTy altOp resultCGType

    closeBlock $ Br
      { dest      = switchExit
      , metadata' = []
      }
    pure (convertedAltOp, lastAltBlock)

  activeBlock curBlockName
  closeBlock $ Switch
    { operand0'   = tagVal
    , defaultDest = if debugMode then mkName "error_block" else switchExit
    , dests       = altDests
    , metadata'   = []
    }

  activeBlock switchExit

  pure . I resultCGType $ Phi
    { type'           = cgLLVMType resultCGType
    , incomingValues  = altConvertedValues ++ if debugMode then [] else [(undef (cgLLVMType resultCGType), curBlockName)]
    , metadata        = []
    }

codeGenTagSwitch tagVal nodeSet tagAltGen | [(tag, items)] <- Map.toList nodeSet = do
  uncurry O <$> tagAltGen tag items

codeGenTagSwitch tagVal nodeSet tagAltGen = error $ "LLVM codegen: empty node set for " ++ show tagVal

-- heap pointer related functions

-- | Allocate memory for a node, dispatching based on GC mode
codeGenIncreaseHeapPointer :: CGType -> CG Operand
codeGenIncreaseHeapPointer varT = do
  gcMode <- gets _envGCMode
  case gcMode of
    GC_BumpAllocator -> codeGenBumpAlloc varT
    GC_Boehm         -> codeGenGCMalloc varT

-- | Bump allocator: increment heap pointer atomically and return old value
codeGenBumpAlloc :: CGType -> CG Operand
codeGenBumpAlloc varT = do
  -- increase heap pointer and return the old value which points to the first free block
  nodeSet <- case varT of
    CG_SimpleType {cgType = T_SimpleType (T_Location locs)} -> mconcat <$> mapM (\loc -> use $ envTypeEnv.location.ix loc) locs
    CG_NodeSet {cgType = T_NodeSet ns} -> pure ns
    _ -> error $ show varT

  let structTy = tuLLVMType $ taggedUnion nodeSet  -- LLVM 15: GEP needs explicit element type
  tuSizePtr <- codeGenLocalVar "alloc_bytes" ptr $ AST.GetElementPtr
    { inBounds  = True
    , type'     = structTy  -- LLVM 15: opaque pointers require explicit type
    , address   = ConstantOperand $ Null { constantType = ptr }
    , indices   = [ConstantOperand $ C.Int 32 1]
    , metadata  = []
    }
  tuSizeInt <- codeGenLocalVar "alloc_bytes" i64 $ AST.PtrToInt
    { operand0  = tuSizePtr
    , type'     = i64
    , metadata  = []
    }
  heapInt <- codeGenLocalVar "new_node_ptr" i64 $ AST.AtomicRMW
    { volatile      = False
    , rmwOperation  = RMWOperation.Add
    , address       = ConstantOperand $ GlobalReference (mkName heapPointerName)  -- LLVM 15: GlobalReference only takes name
    , value         = tuSizeInt
    , alignment     = 8  -- LLVM 15: AtomicRMW requires alignment (8 bytes for i64)
    , atomicity     = (System, Monotonic)
    , metadata      = []
    }
  codeGenLocalVar "new_node_ptr" ptr $ AST.IntToPtr  -- LLVM 15: opaque pointers
    { operand0  = heapInt
    , type'     = ptr  -- LLVM 15: opaque pointers
    , metadata  = []
    }

-- | Boehm GC allocator: call GC_malloc to allocate memory
codeGenGCMalloc :: CGType -> CG Operand
codeGenGCMalloc varT = do
  -- Calculate the allocation size (same as bump allocator)
  nodeSet <- case varT of
    CG_SimpleType {cgType = T_SimpleType (T_Location locs)} -> mconcat <$> mapM (\loc -> use $ envTypeEnv.location.ix loc) locs
    CG_NodeSet {cgType = T_NodeSet ns} -> pure ns
    _ -> error $ show varT

  let structTy = tuLLVMType $ taggedUnion nodeSet  -- LLVM 15: GEP needs explicit element type
  tuSizePtr <- codeGenLocalVar "alloc_bytes" ptr $ AST.GetElementPtr
    { inBounds  = True
    , type'     = structTy  -- LLVM 15: opaque pointers require explicit type
    , address   = ConstantOperand $ Null { constantType = ptr }
    , indices   = [ConstantOperand $ C.Int 32 1]
    , metadata  = []
    }
  tuSizeInt <- codeGenLocalVar "alloc_bytes" i64 $ AST.PtrToInt
    { operand0  = tuSizePtr
    , type'     = i64
    , metadata  = []
    }

  -- Call GC_malloc instead of atomic heap pointer increment
  let gcMallocType = FunctionType
        { resultType    = ptr
        , argumentTypes = [i64]
        , isVarArg      = False
        }
  codeGenLocalVar "gc_ptr" ptr $ AST.Call
    { tailCallKind        = Nothing
    , callingConvention   = CC.C
    , returnAttributes    = []
    , type'               = gcMallocType  -- LLVM 15: requires explicit function type
    , function            = Right . ConstantOperand $ GlobalReference (mkName "GC_malloc")
    , arguments           = [(tuSizeInt, [])]
    , functionAttributes  = []
    , metadata            = []
    }

external :: Type -> AST.Name -> [(Type, AST.Name)] -> CG ()
external retty label argtys = modify' (\env@Env{..} -> env {_envDefinitions = def : _envDefinitions}) where
  def = GlobalDefinition $ functionDefaults
    { name        = label
    , linkage     = L.External
    , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType  = retty
    , basicBlocks = []
    }

-- available primitive functions
registerPrimFunLib :: External -> CG ()
registerPrimFunLib ext = do
  external
    (toLLVMType $ eRetType ext)
    (mkName $ Text.unpack $ unNM $ eName ext)
    [ (toLLVMType t, mkName ("x" ++ show n)) | (t,n) <- (eArgsType ext) `zip` [1..] ]
  where
    toLLVMType = \case
      TySimple t -> typeGenSimpleType t
      rest       -> error $ "Unsupported type:" ++ show rest

runtimeErrorExternal :: CG ()
runtimeErrorExternal =
  external
    (typeGenSimpleType T_Unit)
    (mkName "__runtime_error")
    [(typeGenSimpleType T_Int64, mkName "x0")]

-- | Declare GC_malloc external for Boehm GC
gcMallocExternal :: CG ()
gcMallocExternal =
  external ptr (mkName "GC_malloc") [(i64, mkName "size")]

errorBlock :: CG ()
errorBlock = do
  activeBlock $ mkName "error_block"
  let functionType = FunctionType
                { resultType    = VoidType
                , argumentTypes = [i64]
                , isVarArg      = False
                }

  emit [Do Call
    { tailCallKind        = Just Tail
    , callingConvention   = CC.C
    , returnAttributes    = []
    , type'               = functionType  -- LLVM 15: requires explicit function type
    , function            = Right . ConstantOperand $ GlobalReference (mkName "__runtime_error")  -- LLVM 15: GlobalReference only takes name
    , arguments           = zip [ConstantOperand $ C.Int 64 666] (repeat [])
    , functionAttributes  = []
    , metadata            = []
    }]
  closeBlock $ Unreachable []
