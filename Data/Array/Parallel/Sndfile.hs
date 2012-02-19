{-# LANGUAGE ScopedTypeVariables, RankNTypes, NoImplicitPrelude #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable

Read and write audio file with dph arrays via libsndfile.

-}
module Data.Array.Parallel.Sndfile where

import Data.Array.Parallel.PArray (PArray, PA)
import Sound.File.Sndfile (Count, Info(..), Sample(..))
import Prelude hiding (readFile, writeFile)

import qualified Data.Array.Parallel.PArray as PA
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Sound.File.Sndfile as S
import qualified Sound.File.Sndfile.Buffer.Vector as SV

-- | Read file from given filepath to PArray
--
readFile ::
  forall a. (Sample a, PA a) => FilePath -> IO (Info, Maybe (PArray a))
readFile file = do
  (info, vec) <- fmap (fmap SV.fromBuffer) `fmap`
    S.readFile file :: IO (Info, Maybe (VS.Vector a))
  return (info, (PA.fromVector . V.convert) `fmap` vec)
{-# INLINEABLE readFile #-}
{-# SPECIALISE readFile :: FilePath -> IO (Info, Maybe (PArray Double)) #-}

-- | Write the contents of PArray to given filepath with audio format info.
--
writeFile ::
  forall a. (Sample a, PA a) => Info -> FilePath -> PArray a -> IO Count
writeFile info file arr =
  S.writeFile info file . SV.toBuffer . V.convert . PA.toVector $ arr
{-# INLINEABLE writeFile #-}
{-# SPECIALISE writeFile :: Info -> FilePath -> PArray Double -> IO Count #-}
