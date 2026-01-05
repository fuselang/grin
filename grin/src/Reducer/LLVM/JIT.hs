{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Reducer.LLVM.JIT where

import Grin.Grin (Val(..))
import Reducer.Base (RTVal(..))
import Data.String

import LLVM.Target
import LLVM.Context
import LLVM.Module
import qualified LLVM.AST as AST

import LLVM.OrcJIT

import Control.Monad ((<=<))
import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS
import System.Exit

import Data.Int
import Data.IORef
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import qualified Data.Map.Strict as Map

foreign import ccall "dynamic"
  mkMain :: FunPtr (IO Int64) -> IO Int64

foreign import ccall "wrapper"
  wrapIntPrint :: (Int64 -> IO ()) -> IO (FunPtr (Int64 -> IO ()))

foreign import ccall "wrapper"
  wrapRuntimeError :: (Int64 -> IO ()) -> IO (FunPtr (Int64 -> IO ()))

withTestModule :: AST.Module -> (LLVM.Module.Module -> IO a) -> IO a
withTestModule mod f = withContext $ \context -> withModuleFromAST context mod f

myIntPrintImpl :: Int64 -> IO ()
myIntPrintImpl i = print i

myRuntimeErrorImpl :: Int64 -> IO ()
myRuntimeErrorImpl i = exitWith $ ExitFailure (fromIntegral i)

failInIO :: ExceptT String IO a -> IO a
failInIO = either fail pure <=< runExceptT

grinHeapSize :: Int
grinHeapSize = 100 * 1024 * 1024

-- IMPORTANT: JIT does not support FFI yet, only _prim_int_print and __runtime_error are hardwired
-- NOTE: The LLVM 15 OrcJIT v2 API is significantly different from the old API.
-- This implementation uses the new JITDylib-based approach with defineAbsoluteSymbols
-- for custom symbol resolution.
eagerJit :: AST.Module -> String -> IO RTVal
eagerJit amod mainName = do
  withTestModule amod $ \mod ->
    withHostTargetMachineDefault $ \tm ->
    withExecutionSession $ \es -> do
      -- Create linking and compile layers
      linkingLayer <- createRTDyldObjectLinkingLayer es
      compileLayer <- createIRCompileLayer es linkingLayer tm

      -- Create a JITDylib for our module
      dylib <- createJITDylib es "main"

      -- Add dynamic library search for standard symbols
      addDynamicLibrarySearchGeneratorForCurrentProcess compileLayer dylib

      -- Create function pointers for our custom implementations
      intPrintPtr <- wrapIntPrint myIntPrintImpl
      runtimeErrorPtr <- wrapRuntimeError myRuntimeErrorImpl

      -- Get mangled symbol names and define absolute symbols for our custom functions
      withMangledSymbol compileLayer "_prim_int_print" $ \intPrintSym ->
        withMangledSymbol compileLayer "__runtime_error" $ \runtimeErrorSym -> do
          let intPrintAddr = ptrToWordPtr (castFunPtrToPtr intPrintPtr)
              runtimeErrorAddr = ptrToWordPtr (castFunPtrToPtr runtimeErrorPtr)
              intPrintJitSym = JITSymbol intPrintAddr defaultJITSymbolFlags { jitSymbolExported = True }
              runtimeErrorJitSym = JITSymbol runtimeErrorAddr defaultJITSymbolFlags { jitSymbolExported = True }

          -- Define our custom symbols in the dylib
          defineAbsoluteSymbols dylib
            [ (intPrintSym, intPrintJitSym)
            , (runtimeErrorSym, runtimeErrorJitSym)
            ]

          -- Clone the module as a thread-safe module and add it to the dylib
          withClonedThreadSafeModule mod $ \tsModule -> do
            addModule tsModule dylib compileLayer

            -- Look up the main function
            mainResult <- lookupSymbol es compileLayer dylib (fromString mainName)
            case mainResult of
              Left (JITSymbolError err) -> do
                putStrLn $ "JIT error looking up main: " ++ show err
                pure RT_Unit
              Right (JITSymbol mainFn _) -> do
                -- Look up heap pointer
                heapResult <- lookupSymbol es compileLayer dylib "_heap_ptr_"
                case heapResult of
                  Left (JITSymbolError err) -> do
                    putStrLn $ "JIT error looking up heap pointer: " ++ show err
                    pure RT_Unit
                  Right (JITSymbol heapWordPtr _) -> do
                    -- Allocate GRIN heap
                    heapPointer <- callocBytes grinHeapSize :: IO (Ptr Int8)
                    poke (wordPtrToPtr heapWordPtr :: Ptr Int64) (fromIntegral $ minusPtr heapPointer nullPtr)
                    -- Run function
                    result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
                    -- TODO: read back the result and build the haskell value representation
                    -- Free GRIN heap
                    free heapPointer
                    pure RT_Unit
