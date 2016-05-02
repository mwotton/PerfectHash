{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Data.PerfectHashSpec where

import qualified Data.ByteString.Char8 as S
import qualified Data.List             as List
import qualified Data.Map              as Map
import           Data.Maybe
import qualified Data.PerfectHash      as PerfectHash
import           Debug.Trace
import           Test.Hspec
import           Test.QuickCheck


newtype Blub = Blub ([S.ByteString],[S.ByteString])
    deriving (Show, Read)

newtype Bar = Bar [S.ByteString]
    deriving (Show, Read)

main = hspec spec

spec = describe "PerfectHash" $ do

  it "Successfully looks up valid keys" $ do
    ws <- take 10000 . S.lines <$> S.readFile "/usr/share/dict/words"
    let al = zip ws ws
        Just hash    = PerfectHash.fromList al
        datamap = Map.fromList al
    (`mapM_` ws) $ \w ->
      PerfectHash.lookup w hash `shouldBe` Map.lookup w datamap

  it "doesn't crash on invalid keys" $ do
    ws <- take 10000 . S.lines <$> S.readFile "/usr/share/dict/words"
    let al = zip ws ws
        Just hash    = PerfectHash.fromList al
    PerfectHash.lookup "plerfle" hash `shouldSatisfy`
        (\x -> isJust x || isNothing x)


halves :: [a] -> ([a], [a])
halves ls = List.splitAt ((length ls) `div` 2) ls
