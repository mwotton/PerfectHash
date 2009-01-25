
{-# LANGUAGE ScopedTypeVariables #-}

import Random
import HackedMicrobench -- small bugfix - use Integer rather than Int. original author unavailable for reports
import Control.Exception
import Data.List as List
import Data.PerfectHash as PerfectHash
import Maybe
import qualified Data.ByteString.Char8 as S


perfect_lookup :: PerfectHash Integer -> [S.ByteString] -> Integer -> Integer
perfect_lookup m words size = sum $ mapMaybe (PerfectHash.lookup m) targets
 where targets :: [S.ByteString] = genericTake size $ cycle words

main = do
 str <- S.readFile "/usr/share/dict/words"
 words <- evaluate $ S.lines str
 source :: [(S.ByteString, Integer)] <- evaluate $ zip words [1..]
 perfect <- evaluate $ PerfectHash.fromList source
 {-# SCC "mb_lookup" #-} microbench "perfect lookup" (perfect_lookup   perfect words)