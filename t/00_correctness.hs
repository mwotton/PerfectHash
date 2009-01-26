{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, IncoherentInstances, OverlappingInstances  #-}

import qualified Data.ByteString.Char8 as S
import qualified Data.Map as Map
import qualified Data.PerfectHash as PerfectHash
import Debug.Trace
import Test.HUnit
import List 
import Maybe
import Test.QuickCheck


newtype Blub = Blub ([S.ByteString],[S.ByteString])
    deriving (Show, Read)

newtype Bar = Bar [S.ByteString]
    deriving (Show, Read)


-- would be nice to have a reasonable way of generating random strings, then i could 
-- move this to quickcheck and get rid of the dependence on /usr/share/dict/words

halves :: [a] -> ([a], [a])
halves ls = List.splitAt ((length ls) `div` 2) ls

big_test ws = TestList [TestLabel "Successful lookup"          $ TestList successTests, 
                        TestLabel "Faithful report of absence" $ TestList absenceTests]
  where hash    = PerfectHash.fromList al
        datamap = Map.fromList al
        al = zip source [0..]
        (source, orphans) =  halves $ sortedNub ws
        absenceTests = map (\w -> TestCase $
                                  assertEqual "shouldn't find it"
                                                  (PerfectHash.lookup hash w)
                                                  Nothing
                           ) orphans
        successTests = map (\w -> TestCase $
                                  assertEqual "should find it: same result as Data.Map"
                                                  (Map.lookup w datamap )
                                                  (PerfectHash.lookup hash w)

                           ) source
        
sortedNub xs = map head $ group $ sort xs

main = do
  l <- S.readFile "/usr/share/dict/words";
  runTestTT (big_test $ take 10000 $ S.lines l)
  