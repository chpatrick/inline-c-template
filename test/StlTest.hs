{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Templates as CT
import qualified VectorTemplate as Vector
import qualified MapTemplate as UnorderedMap
import Data.Foldable

import Language.Haskell.TH

import Foreign
import Foreign.C

C.context C.cppCtx

CT.instantiate Vector.stdVectorTemplate [ ( "int", [t|CInt|] ) ]
CT.instantiate Vector.stdVectorTemplate [ ( "float", [t|CFloat|] ) ]
CT.instantiate Vector.stdVectorTemplate [ ( "void*", [t|Ptr ()|] ) ]

CT.instantiate UnorderedMap.stdUnorderedMapTemplate [ ( "int", [t|CInt|] ), ( "float", [t|CFloat|] ) ]

newtype BoxedVector a = BoxedVector (Vector.StdVector (Ptr ()))

newBoxed :: IO (BoxedVector a)
newBoxed = BoxedVector <$> Vector.new

pushBackBoxed :: BoxedVector a -> a -> IO ()
pushBackBoxed (BoxedVector vec) x = do
  sptr <- newStablePtr x
  Vector.pushBack vec (castStablePtrToPtr sptr)

atBoxed :: BoxedVector a -> Int -> IO a
atBoxed (BoxedVector vec) ix = do
  ptr <- Vector.at vec ix
  deRefStablePtr (castPtrToStablePtr ptr)

main :: IO ()
main = do
  cVector <- Vector.new @CInt
  Vector.pushBack cVector 1
  Vector.pushBack cVector 2
  for_ [0..1] $ \ix -> do
    val <- Vector.at cVector ix
    print val

  floatVector <- Vector.new @CFloat
  Vector.pushBack floatVector pi
  Vector.pushBack floatVector (exp 1)
  for_ [0..1] $ \ix -> do
    val <- Vector.at floatVector ix
    print val

  stringVector <- newBoxed @String
  pushBackBoxed stringVector "foo"
  pushBackBoxed stringVector "bar"
  for_ [0..1] $ \ix -> do
    val <- atBoxed stringVector ix
    print val

  intFloatMap <- UnorderedMap.new @CInt @CFloat
  UnorderedMap.put intFloatMap 42 pi
  UnorderedMap.put intFloatMap 666 (sqrt 2)
  UnorderedMap.get intFloatMap 42 >>= print
  UnorderedMap.get intFloatMap 666 >>= print