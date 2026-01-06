{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Reducer.LLVM.CodeGenSpec where

import Test.Hspec

import LLVM.AST (Module(..), Definition(..))
import LLVM.AST.Global (Global(..))
import qualified LLVM.AST as AST

import Grin.Grin
import Grin.TH
import Grin.TypeCheck (inferTypeEnv)
import Grin.TypeEnv (TypeEnv)
import Grin.PrimOpsPrelude (withPrimPrelude)

import Reducer.LLVM.Base (GCMode(..))
import Reducer.LLVM.CodeGen (codeGen, codeGenWithGC)


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = describe "LLVM CodeGen GC Mode" $ do

  describe "GC_BumpAllocator mode" $ do
    it "does not include GC_malloc declaration" $ do
      let mod = codeGenWithGC GC_BumpAllocator testTypeEnv testProgram
      hasGCMallocDeclaration mod `shouldBe` False

    it "includes _heap_ptr_ global variable" $ do
      let mod = codeGenWithGC GC_BumpAllocator testTypeEnv testProgram
      hasHeapPointerGlobal mod `shouldBe` True

  describe "GC_Boehm mode" $ do
    it "includes GC_malloc declaration" $ do
      let mod = codeGenWithGC GC_Boehm testTypeEnv testProgram
      hasGCMallocDeclaration mod `shouldBe` True

    it "does not include _heap_ptr_ global (not needed with GC)" $ do
      let mod = codeGenWithGC GC_Boehm testTypeEnv testProgram
      hasHeapPointerGlobal mod `shouldBe` False

  describe "default codeGen" $ do
    it "uses bump allocator by default" $ do
      let mod = codeGen testTypeEnv testProgram
      hasGCMallocDeclaration mod `shouldBe` False


-- | Check if the module contains a GC_malloc external function declaration
hasGCMallocDeclaration :: AST.Module -> Bool
hasGCMallocDeclaration mod =
  any isGCMallocDecl (moduleDefinitions mod)
  where
    isGCMallocDecl (GlobalDefinition f@Function{}) =
      name f == AST.mkName "GC_malloc"
    isGCMallocDecl _ = False

-- | Check if the module contains the _heap_ptr_ global variable
hasHeapPointerGlobal :: AST.Module -> Bool
hasHeapPointerGlobal mod =
  any isHeapPtrGlobal (moduleDefinitions mod)
  where
    isHeapPtrGlobal (GlobalDefinition g@GlobalVariable{}) =
      name g == AST.mkName "_heap_ptr_"
    isHeapPtrGlobal _ = False


-- | A simple test program with a store operation (which triggers allocation)
-- Uses withPrimPrelude to ensure _prim_int_print is defined
testProgram :: Exp
testProgram = withPrimPrelude [prog|
  grinMain =
    p <- store (CInt 42)
    (CInt n) <- fetch p
    _prim_int_print n
  |]

-- | Create type environment for the test program using HPT analysis
testTypeEnv :: TypeEnv
testTypeEnv = inferTypeEnv testProgram
