basic model here: mar

> {-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
> module PerfectHash ( PerfectHash, fromList, lookup ) where

Arguably the FFI stuff should be in a separate file, but let's keep it simple for the moment.

> import Array
> import Data.Array.IO
> import Foreign
> import Foreign.C.String
> import Foreign.C.Types
> import Foreign.Marshal.Array
> import Prelude hiding (lookup)
> import System.IO.Unsafe -- naughty!
> import qualified Data.ByteString.Char8 as S
> import Debug.Trace

> foreign import ccall safe "stub.h stub_hash" c_stub_hash   :: Ptr ForeignHash -> CString -> CULong
> foreign import ccall safe "stub.h build_hash" c_build_hash :: Ptr CString -> CInt -> IO (Ptr ForeignHash)

> type ForeignHash = ()

> data PerfectHash a = PerfectHash { store :: Array Word64 a,
>                                    hashFunc :: S.ByteString -> Word64 }


> use_hash a str = unsafePerformIO $ S.useAsCString str (\cstr -> return (fromIntegral $ c_stub_hash a cstr))

> conv :: S.ByteString -> IO CString
> conv x = S.useAsCString x (return)


> writeToArray :: Storable a => [a] -> IO (Ptr a)
> writeToArray l = do
>   ptr <- mallocBytes (4 * length l)
>   mapM_ (\(index,element) -> pokeByteOff ptr (4*index) element ) (zip [0..] l)
>   return ptr


                   withArray2 cstrs $ \c_input -> do

> fromList :: Show a => [(S.ByteString, a)] -> IO (PerfectHash a)
> fromList ls = trace ("building hash: first ten are " ++ show (take 10 ls))  $ do
>                   cstrs <- mapM (conv . fst) ls
>                   ptr <- writeToArray cstrs
>                   let len = length cstrs
>                   cmph <- c_build_hash  ptr (fromIntegral len)
>                   let bounds :: (Word64, Word64) = (fromIntegral 0, fromIntegral len)
>                   arr <- newArray_ bounds :: IO (IOArray Word64 a)
>                   let hashFunc = use_hash cmph
>                   mapM_ (\(str,e) -> writeArray arr (hashFunc str) e) ls
>                   immutable <- freeze arr
>                   return PerfectHash { store = immutable, hashFunc = hashFunc }
>                 

> lookup :: PerfectHash a -> S.ByteString -> Maybe a
> lookup hash bs
>     | low <= index && high > index = Just (arr ! index)
>     | otherwise                     = trace (S.unpack bs)  Nothing
>     where index = hashFunc hash bs
>           (low, high) = bounds arr
>           arr = store hash