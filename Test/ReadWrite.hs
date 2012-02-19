module Test.ReadWrite where

import Data.Array.Parallel.PArray (PArray)
import Sound.File.Sndfile (Info(..))
import System.Environment (getArgs)
import qualified Data.Array.Parallel.PArray as PA
import qualified Sound.File.Sndfile as S

import Test.Vectorised
import qualified Data.Array.Parallel.Sndfile as PS

-- --------------------------------------------------------------------------
-- Tests

test_readFile :: FilePath -> IO Int
test_readFile file = do
  (_, pa) <- PS.readFile file
  return $ maybe 0 (PA.length :: PArray Double -> Int) pa

test_writeFile :: FilePath -> IO ()
test_writeFile file = do
  let sr = samplerate wav16
  ns <- PS.writeFile wav16 file (gensinesPA 110 (sr*3) sr)
  putStrLn $ unwords ["Wrote", show ns, "samples to", file]

wav16 :: S.Info
wav16 = S.Info
  { samplerate = 48000
  , channels = 1
  , frames = 0
  , format = S.Format S.HeaderFormatWav S.SampleFormatPcm16 S.EndianFile
  , sections = 1
  , seekable = True }

usage :: IO ()
usage = mapM_ putStrLn
  [ "Usage:"
  , "  read  FILEPATH - read given file and print number of read samples"
  , "  write FILEPATH - write sine wav to given file"
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    "read":file:_  -> print =<< test_readFile file
    "write":file:_ -> test_writeFile file
    _              -> usage
