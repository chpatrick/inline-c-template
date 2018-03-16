{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MapTemplate
  ( StdUnorderedMap(..)
  , StdUnorderedMapImpl(..)
  , stdUnorderedMapTemplate
  ) where

import qualified Language.C.Inline.Cpp.Templates as CT

import Language.Haskell.TH
import qualified Language.C.Inline.Cpp as C

import Foreign
import qualified Foreign.Concurrent as FC
import Foreign.C

newtype StdUnorderedMap k v = StdUnorderedMap (ForeignPtr ())

class StdUnorderedMapImpl k v where
  new :: IO (StdUnorderedMap k v)
  put :: StdUnorderedMap k v -> k -> v -> IO ()
  get :: StdUnorderedMap k v -> k -> IO v

stdUnorderedMapTemplate :: CT.Template
stdUnorderedMapTemplate = CT.Template
  { CT.templatePreDecs = [ C.include "<unordered_map>" ]
  , CT.templateInstanceDec = [d|
      instance StdUnorderedMapImpl k v where
        new = do
          mapPtr <- [CT.exp| void* { new std::unordered_map<@k, @v>() } |]
          let destructor = [CT.exp| void { delete reinterpret_cast<std::unordered_map<@k, @v>*>($(void* mapPtr)) } |]
          StdUnorderedMap <$> FC.newForeignPtr mapPtr destructor

        put (StdUnorderedMap fptr) key value = withForeignPtr fptr $ \ptr -> do
          [CT.exp| void {
            reinterpret_cast<std::unordered_map<@k, @v>*>($(void* ptr))->insert({ $(@k key), $(@v value) })
          }|]

        get (StdUnorderedMap fptr) key = withForeignPtr fptr $ \ptr -> do
          [CT.exp| @v {
            reinterpret_cast<std::unordered_map<@k, @v>*>($(void* ptr))->at($(@k key))
          }|]
    |]
  }