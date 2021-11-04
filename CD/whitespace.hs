import System.Environment (getArgs)
import System.Exit (exitFailure)
main :: IO ()
main = do
    args <- getArgs
    if length args < 2 then exitFailure 
    else do
        let (a: b: xs) = args
        inp <- readFile a
        let (outh,_) =  foldr mango ([],' ') inp-- removeWS (reverse inp) []
        writeFile b outh

mango :: Char -> ([Char], Char) -> ([Char], Char)
mango x (acc, last)
    | isWhitespace x && isWhitespace last = (acc, last)
    | isWhitespace x = (' ':acc, x)
    | otherwise = (x:acc, x)
    where isWhitespace x = x == ' ' || x == '\t'