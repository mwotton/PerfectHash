> {-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, EmptyDataDecls, BangPatterns, TypeSynonymInstances, FlexibleInstances #-}

> module Data.PerfectHash ( PerfectHash, fromList, lookup, lookupByIndex ) where
>
> import Data.Array.Unsafe (unsafeFreeze)
> import System.IO.Unsafe (unsafePerformIO)
> import Data.Array
> import Data.Array.IO
> import Foreign(Ptr)
> import Foreign.C.String
> import Foreign.C.Types
> import Foreign.Marshal.Array
> import Prelude hiding (lookup)
> import qualified Data.ByteString.Char8 as S
> import qualified Data.ByteString.Unsafe as Unsafe
> import Data.Array.Storable
> import Data.Binary
> import Data.Digest.CRC32 (crc32)
> import Data.Digest.Adler32 (adler32)
> import Control.Monad(guard, liftM)
> import GHC.Arr (unsafeAt)
 
Arguably the FFI stuff should be in a separate file, but let's keep it simple for the moment.

> foreign import ccall unsafe "cmph.h cmph_search" c_cmph_search :: Ptr ForeignHash -> CString -> CInt -> CULong
> foreign import ccall unsafe "stub.h build_hash" c_build_hash   :: Ptr CString -> CInt -> IO (Ptr ForeignHash)
> foreign import ccall unsafe "string.h strdup" c_strdup         :: CString -> IO CString
> foreign import ccall unsafe "string.h strncmp" c_strncmp         :: CString -> CString -> CInt -> IO CInt

standard idiom for an opaque type

> data ForeignHash

> data PerfectHash a = PerfectHash { store     :: !(Array Word32 (a,CString)),
>                                    cmph      :: Ptr ForeignHash
>                                  }

is this even a sane thing to do?

-- > instance Binary CString where
-- >     get = undefined
-- >     put (s :: CString) = undefined

-- > instance (Binary a) => Binary (PerfectHash a) where
-- >     get = do
-- >           size <- get
-- >           arr <- newArray_ (0,size)
-- >           forM_ [0 .. size] $ writeArray arr
-- >           store <- get
-- >           cmph <- undefined
-- >           return $ PerfectHash { store = store, cmph = cmph }
-- >     put p = do
-- >             let arr = store p
-- >                 last = rangeSize (bounds arr) - 1
-- >             put last
-- >             mapM_ (\(a, str) -> put a >> put (unsafePerformIO $ S.packCString str))
-- >                       [ unsafeAt arr i | i <- [0 .. last] ]

-- >             put (cmph p)

-- > instance Binary (Ptr ForeignHash) where
-- >     get = undefined
-- >     put f = undefined

> raw_hashfunc hash (cstr,len) = fromIntegral $ c_cmph_search hash cstr (fromIntegral len)

This could do with being broken up a little, probably


> fromList :: Show a => [(S.ByteString, a)] -> PerfectHash a
> fromList ls = unsafePerformIO $ do
>                   (fodder,cstr_ptrs) <- prepareCPtrs
>                   cmph <- withStorableArray fodder $ \ptr -> c_build_hash ptr (fromIntegral len)
>                   i_arr <- buildArray cmph cstr_ptrs
>                   return PerfectHash { store = i_arr,
>                                        cmph = cmph }

>   where prepareCPtrs = do
>                   fodder <- newArray_ (0, len-1)
>                   -- we make one pass over ls, then throw it away
>                   cstr_ptrs <- mapM (\(i,(bs, val)) ->
>                          S.useAsCStringLen bs $ \(cstr,len) -> do
>                            newPtr <- c_strdup cstr
>                            writeArray fodder i newPtr
>                            return ((newPtr,fromIntegral len),val))
>                         (zip [0..] ls)
>                   return (fodder, cstr_ptrs)
>
>         buildArray cmph cstr_ptrs = do
>                   arr <- newArray_ (fromIntegral 0, fromIntegral len - 1) :: IO (IOArray Word32 a)
>                   mapM_ (\(cl@(cstr,len),val) -> writeArray arr (raw_hashfunc cmph cl) (val,cstr)) $ cstr_ptrs
>                   -- we created it, we can do what we like with it...
>                   unsafeFreeze arr
>
>         len = length ls

> lookup :: PerfectHash a -> S.ByteString -> Maybe a
> lookup !hash !bs = guard check >> return e
>     where index = {-# SCC "hash_only" #-} use_hash (cmph hash) bs
>           (!low, !high) = bounds arr
>           !arr = store hash
>           (e, str) = arr ! index

basic index checking stuff plus a check that we haven't just had a hash collision - tried crc checking here
and it turns out that strncmp is faster

>           !check = {-# SCC "check" #-} low <= index && high >= index &&
>                   {-# SCC "bs_check" #-} unsafePerformIO $ Unsafe.unsafeUseAsCStringLen bs (\(cstr,len) ->
>                                               c_strncmp str cstr (fromIntegral len) >>= \res -> return (res == 0))

>           use_hash a str = {-# SCC "use_hash" #-}  unsafePerformIO $ Unsafe.unsafeUseAsCStringLen str
>                            (return . raw_hashfunc a)




sometimes it's convenient to have direct access, but this should probably be seen as a
slightly devious use case.

> lookupByIndex :: PerfectHash a -> Word32 -> Maybe S.ByteString
> lookupByIndex hash index = do
>                guard $ low <= index && high >= index
>                return $ unsafePerformIO $ S.packCString $ snd $ unsafeAt arr (fromIntegral index)
>   where (!low, !high) = bounds arr
>         arr = store hash
