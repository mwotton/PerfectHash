import qualified Data.PerfectHash as PerfectHash
import qualified Data.ByteString.Char8 as S
import System (getArgs)

main = do
    (num:_) <- getArgs
    str <- S.readFile "/usr/share/dict/words"
    let ws = take (read num) $ S.lines str
    putStrLn $ "Last is: " ++ (S.unpack $last ws)

    let hash = PerfectHash.fromList (zip ws [1..] ) 
    print $ PerfectHash.lookup hash  (S.pack "fortune")
