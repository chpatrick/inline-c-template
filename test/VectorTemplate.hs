{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module VectorTemplate
  ( StdVector(..)
  , StdVectorImpl(..)
  , stdVectorTemplate
  ) where

import qualified Language.C.Inline.Cpp.Templates as CT

import Language.Haskell.TH

import Foreign
import Foreign.C

newtype StdVector a = StdVector (ForeignPtr ())

class StdVectorImpl a where
  new :: IO (StdVector a)

stdVectorTemplate :: CT.Template
stdVectorTemplate = CT.template [d|
  instance StdVectorImpl a where
    new = do
      vecPtr <- [CT.block| void* { return new std::vector<@(a)>(); } |]
      StdVector <$> newForeignPtr_ vecPtr
  |]
