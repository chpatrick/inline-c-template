{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module VectorTemplate
  ( StdVector(..)
  , StdVectorImpl(..)
  , stdVectorTemplate
  ) where

import qualified Language.C.Inline.Cpp.Templates as CT

import Language.Haskell.TH
import qualified Language.C.Inline.Cpp as C

import Foreign
import qualified Foreign.Concurrent as FC
import Foreign.C

newtype StdVector a = StdVector (ForeignPtr ())

class StdVectorImpl a where
  new :: IO (StdVector a)
  pushBack :: StdVector a -> a -> IO ()
  at :: StdVector a -> Int -> IO a

stdVectorTemplate :: CT.Template
stdVectorTemplate = CT.Template
  { CT.templatePreDecs = [ C.include "<vector>" ]
  , CT.templateInstanceDec = [d|
    instance StdVectorImpl a where
      new = do
        vecPtr <- [CT.exp| void* { new std::vector<@a>() } |]
        let destructor = [CT.exp| void { delete reinterpret_cast<std::vector<@a>*>($(void* vecPtr)) } |]
        StdVector <$> FC.newForeignPtr vecPtr destructor

      pushBack (StdVector fptr) x = withForeignPtr fptr $ \ptr ->
        [CT.block| void {
          reinterpret_cast<std::vector<@a>*>($(void* ptr))->push_back($(@a x));
        }|]

      at (StdVector fptr) ix = withForeignPtr fptr $ \ptr -> do
        let cIx = fromIntegral ix
        [CT.exp| @a {
          reinterpret_cast<std::vector<@a>*>($(void* ptr))->at($(int cIx))
        }|]
    |]
  }
