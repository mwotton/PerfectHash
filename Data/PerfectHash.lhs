> {-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, EmptyDataDecls #-}
> module Data.PerfectHash ( PerfectHash, fromList, lookup ) where

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
 
Arguably the FFI stuff should be in a separate file, but let's keep it simple for the moment.

> foreign import ccall unsafe "cmph.h cmph_search" c_cmph_search :: Ptr ForeignHash -> CString -> CInt -> CULong
> foreign import ccall unsafe "stub.h build_hash" c_build_hash   :: Ptr CString -> CInt -> IO (Ptr ForeignHash)
> foreign import ccall unsafe "string.h strdup" c_strdup         :: CString -> IO CString

standard idiom for an opaque type

> data ForeignHash

> data PerfectHash a = PerfectHash { store :: Array Word64  a,
>                                    checksums :: Array Word64 Word32,
>                                    hashFunc :: S.ByteString -> Word64 }

pretty certain this could be improved

> use_hash a str = {-# SCC "use_hash" #-}  unsafePerformIO $ Unsafe.unsafeUseAsCStringLen str
>                  (\(cstr,i) -> return (fromIntegral $ c_cmph_search a cstr (fromIntegral i)))

This could do with being broken up a little, probably

> fromList :: Show a => [(S.ByteString, a)] -> PerfectHash a
> fromList ls = unsafePerformIO $ do
>                   let len = length ls
>                   arr <- newArray_ (0, len-1)
>                   mapM_ (\(i,(bs, val)) ->
>                          S.useAsCString bs $ \cstr -> do
>                            newPtr <- c_strdup cstr
>                            writeArray arr i newPtr)
>                         (zip [0..] ls)
>                   cmph <- withStorableArray arr $ \ptr -> c_build_hash ptr (fromIntegral len)
>                   let bounds :: (Word64, Word64) = (fromIntegral 0, fromIntegral len - 1)
>                   print bounds
>                   arr <- newArray_ bounds :: IO (IOArray Word64 a)
>                   checksums <- newArray_ bounds :: IO (IOArray Word64 Word32)
>                   let hashFunc = use_hash cmph
>                   mapM_ (\(str,e) -> do
>                          let index = hashFunc str
>                          writeArray arr index e
>                          writeArray checksums index (crc32 str))
>                                      ls
>                   i_arr <- freeze arr
>                   i_checksums <- freeze checksums
>                   return PerfectHash { store = i_arr, 
>                                        hashFunc = hashFunc, 
>                                        checksums = i_checksums }
>                 

> lookup :: PerfectHash a -> S.ByteString -> Maybe a
> lookup hash bs
>     | check     = Just (arr ! index)
>     | otherwise = Nothing
>     where index = {-# SCC "hash_only" #-} hashFunc hash bs
>           (low, high) = bounds arr
>           arr = store hash
>           check = {-# SCC "check" #-} low <= index && high >= index && 
>                   {-# SCC "crcs" #-} (crc32 bs) == (checksums hash) ! index