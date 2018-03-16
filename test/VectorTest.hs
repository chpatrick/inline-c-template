{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Templates as CT
import qualified VectorTemplate as Vector
import Data.Foldable

import Language.Haskell.TH

import Foreign
import Foreign.C

C.context C.cppCtx

CT.instantiate Vector.stdVectorTemplate [ ( "int", [t|CInt|] ) ]
CT.instantiate Vector.stdVectorTemplate [ ( "float", [t|CFloat|] ) ]

main :: IO ()
main = do
  cVector <- Vector.new @CInt
  Vector.pushBack cVector 1
  Vector.pushBack cVector 2
  for_ [0..1] $ \ix -> do
    val <- Vector.index cVector ix
    print val

  floatVector <- Vector.new @CFloat
  Vector.pushBack floatVector pi
  Vector.pushBack floatVector (exp 1)
  for_ [0..1] $ \ix -> do
    val <- Vector.index floatVector ix
    print val