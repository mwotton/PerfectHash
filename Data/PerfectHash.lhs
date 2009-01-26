> {-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, EmptyDataDecls, BangPatterns #-}
> module Data.PerfectHash ( PerfectHash, fromList, lookup, lookupByIndex ) where

> import Array
> import Data.Array.IO
> import Foreign
> import Foreign.C.String
> import Foreign.C.Types
> import Foreign.Marshal.Array
> import Prelude hiding (lookup)
> import System.IO.Unsafe
> import qualified Data.ByteString.Char8 as S
> import qualified Data.ByteString.Unsafe as Unsafe
> import Data.Array.Storable
> import Data.Digest.CRC32 (crc32)
> import Data.Digest.Adler32 (adler32)
> import Control.Monad(guard)
 
Arguably the FFI stuff should be in a separate file, but let's keep it simple for the moment.

> foreign import ccall unsafe "cmph.h cmph_search" c_cmph_search :: Ptr ForeignHash -> CString -> CInt -> CULong
> foreign import ccall unsafe "stub.h build_hash" c_build_hash   :: Ptr CString -> CInt -> IO (Ptr ForeignHash)
> foreign import ccall unsafe "string.h strdup" c_strdup         :: CString -> IO CString
> foreign import ccall unsafe "string.h strncmp" c_strncmp         :: CString -> CString -> CInt -> IO CInt

standard idiom for an opaque type

> data ForeignHash

> data PerfectHash a = PerfectHash { store     :: !(Array.Array Word32 (a,CString)),
>                                    hashFunc  :: !(S.ByteString -> Word32) }

pretty certain this could be improved

> use_hash a str = {-# SCC "use_hash" #-}  unsafePerformIO $ Unsafe.unsafeUseAsCStringLen str
>                  (\(cstr,i) -> return (fromIntegral $ c_cmph_search a cstr (fromIntegral i)))

> raw_hashfunc hash cstr len = fromIntegral $ c_cmph_search hash cstr len

This could do with being broken up a little, probably

> fromList :: Show a => [(S.ByteString, a)] -> PerfectHash a
> fromList ls = unsafePerformIO $ do
>                   let len = length ls
>                   arr <- newArray_ (0, len-1)
>                   -- we make one pass over ls, then throw it away
>                   cstr_ptrs <- mapM (\(i,(bs, val)) ->
>                          S.useAsCStringLen bs $ \(cstr,len) -> do
>                            newPtr <- c_strdup cstr
>                            writeArray arr i newPtr
>                            return (newPtr,fromIntegral len,val))
>                         (zip [0..] ls)
>                   cmph <- withStorableArray arr $ \ptr -> c_build_hash ptr (fromIntegral len)
>                   let bounds :: (Word32, Word32) = (fromIntegral 0, fromIntegral len - 1)
>                   arr <- newArray_ bounds :: IO (IOArray Word32 a)
>                   checksums <- newArray_ bounds :: IO (IOArray Word32 Word32)
>                   let hashFunc = use_hash cmph
>                   mapM_ (\(cstr,len,val) -> do
>                          let index = raw_hashfunc cmph cstr len
>                          writeArray arr index (val,cstr)) $ cstr_ptrs
>                   i_arr <- freeze arr
>                   return PerfectHash { store = i_arr, 
>                                        hashFunc = use_hash cmph }

>                 

-- > crc = crc32

-- > crc = adler32

> lookup :: PerfectHash a -> S.ByteString -> Maybe a
> lookup !hash !bs
>     | check     = Just e
>     | otherwise = Nothing
>     where index = {-# SCC "hash_only" #-} hashFunc hash bs
>           (!low, !high) = Array.bounds arr
>           !arr = store hash
>           (e, str) = arr ! index
>           !check = {-# SCC "check" #-} low <= index && high >= index && 
>                   {-# SCC "bs_check" #-} unsafePerformIO $ Unsafe.unsafeUseAsCStringLen bs (\(cstr,len) -> 
>                                               c_strncmp str cstr (fromIntegral len) >>= \res -> return (res == 0))

sometimes it's convenient to have direct access, but this should probably be seen as a
slightly devious use case.

> lookupByIndex :: PerfectHash a -> Word32 -> Maybe S.ByteString
> lookupByIndex hash index = do
>                guard $ low <= index && high >= index
>                return $ unsafePerformIO $ S.packCString $ snd $  arr ! index
>   where (!low, !high) = bounds arr
>         arr = store hash