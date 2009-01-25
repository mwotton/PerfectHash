{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, IncoherentInstances, OverlappingInstances  #-}

import Data.PerfectHash as PerfectHash
import Test.QuickCheck
import List hiding (lookup)
import Maybe
import Prelude hiding (lookup)
import qualified Data.ByteString.Char8 as S
import Debug.Trace

newtype Blub = Blub ([S.ByteString],[S.ByteString])
    deriving (Show, Read)

newtype Bar = Bar [S.ByteString]
    deriving (Show, Read)



instance Arbitrary Blub where
  arbitrary     = return $ Blub $ List.splitAt ((length uniques) `div` 2) uniques 
    where   uniques = map ( S.pack ) ["foo", "bar", "baz"]
    
instance Arbitrary Bar where
  arbitrary     = return $ Bar $ map ( S.pack ) ["foo", "bar", "baz", "boo"]

prop_Working :: Bar -> Bool
prop_Working (Bar ls) = all (\x -> Just x == lookup h x) ls
    where h = fromList $ zip ls ls

prop_Honest :: Blub ->  Bool
prop_Honest (Blub (uniques,targets)) = all (\x -> trace (show x) (Maybe.isNothing x)) results
    where h = fromList $ zip uniques uniques
          results = map (\x -> trace (show x) $ PerfectHash.lookup h x) targets

main = do
  --l <- S.readFile "/usr/share/dict/words";
  --res <- prop_Honest (S.lines l)
  --print res
  quickCheck prop_Honest
  quickCheck prop_Working