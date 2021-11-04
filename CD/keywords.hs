import System.Environment (getArgs)
import Data.Set (Set, fromList, member)
import Data.Char (toUpper)
import System.Exit (exitFailure)
main :: IO ()
main = do
    args <- getArgs
    if null args then exitFailure
    else do
        let (inp:rest) = args
        str <- readFile inp
        (putStr . unlines . filterKeyWord . words) str

filterKeyWord = filter (\x -> member (map toUpper x) keys)
    where keys = fromList ["INT", "SHORT", "LONG", "CHAR", "FOR", "IF", "WHILE"
                ,"RETURN", "VOID", "MAIN"]