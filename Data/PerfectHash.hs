{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.PerfectHash ( PerfectHash, fromList, lookup) where

import           Control.Monad         (guard)
import           Data.Array
import           Data.Array.IO         (IOArray, newArray_, writeArray)
import           Data.Array.Unsafe     (unsafeFreeze)
import qualified Data.ByteString.Char8 as S
import           Data.CMPH             (CMPH)
import qualified Data.CMPH             as CMPH
import           Data.Word
import           Prelude               hiding (lookup)
import           System.IO.Unsafe      (unsafePerformIO)

data PerfectHash a =
  PerfectHash
  { computedHash :: !CMPH
  , store        :: !(Array Word64 a)
  }

fromList :: [(S.ByteString, a)] -> Maybe (PerfectHash a)
fromList ls = unsafePerformIO $ do
  cmph' <- CMPH.fromList (map fst ls)
  case cmph' of
   Nothing -> return Nothing
   Just cmph -> Just <$> do
     (arr :: IOArray Word64 a) <- (newArray_ (0, fromIntegral (CMPH.size cmph - 1) ))
     (`mapM_` ls) $ \(bs,val) -> do
       h <- CMPH.hash  cmph bs
       writeArray arr h val
     u <- unsafeFreeze arr
     return $ PerfectHash cmph u

-- | this is a maybe, because we can't guarantee that
--   the provided string is one of the things that we originally hashed
--   with. We don't try to verify that it matched, because we don't want
--   to store the original string: if you try a string we've never
--   seen, you might get Nothing and you might get a random value.
lookup :: S.ByteString -> PerfectHash a -> Maybe a
lookup !bs !ph  = guard check >> return e
    where index = {-# SCC "hash_only" #-} unsafePerformIO $ CMPH.hash (computedHash ph) bs
          (!low, !high) = bounds arr
          !arr = store ph
          e = {-# SCC array_lookup #-} arr ! index
          !check = {-# SCC "check" #-} low <= index && high >= index
