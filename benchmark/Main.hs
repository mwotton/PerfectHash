{-# LANGUAGE OverloadedStrings #-}
import           Control.DeepSeq
import           Control.Exception     (bracket_, evaluate)
import           Control.Monad         (forM)
import           Criterion.Main
import           Data.Array.IO         (IOArray, newListArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashMap.Strict   as HashMap
import qualified Data.List             as DL
import qualified Data.Map              as Map
import qualified Data.PerfectHash      as PH
import           GHC.Profiling

import           System.Random

-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

main = do
  stopProfTimer
  ws <- BS8.lines <$> BS8.readFile "/usr/share/dict/words"
  let al = zip ws ([1..] :: [Int])
      m = Map.fromList al
      hs = HashMap.fromList al
      -- let us assume that the dictionary contains no embedded nulls.
      Just ph = PH.fromList al
  randomised <- evaluate . force =<< shuffle (concat $ replicate 3 ws)
  evaluate $ force m
  evaluate $ force hs
  evaluate $ force hs
  defaultMain [
    bgroup "fib" [ bench "Data.Map"          $ nf (map (`Map.lookup` m)) randomised
                 , bench "Data.PerfectHash"  $ nf (map (`PH.lookup` ph)) randomised
                   -- bracket_ startProfTimer stopProfTimer

                 , bench "Data.HashMap"      $ nf (map (`HashMap.lookup` hs)) randomised
--                 , bench "Data.List"         $ whnf (`DL.lookup` al) "exorbitant"
                 ]
    ]
