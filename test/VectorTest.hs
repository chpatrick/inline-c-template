{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Language.C.Inline.Cpp as C
import Language.C.Inline.Cpp.Templates
import VectorTemplate

import Language.Haskell.TH

import Foreign
import Foreign.C

C.context C.cppCtx

C.include "<vector>"

instantiate stdVectorTemplate [ ( "int", [t|CInt|] ) ]

main :: IO ()
main = return ()