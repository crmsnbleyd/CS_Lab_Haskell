import System.Environment (getArgs)
import System.Exit (exitFailure)
main :: IO ()
main = do
    args <- getArgs
    if length args < 2 then exitFailure 
    else do
        let (a: b: xs) = args
        inp <- readFile a
        let outh = removeWS (reverse inp) []
        writeFile b outh

removeWS :: [Char] -> [Char] -> [Char]
removeWS [] acc = acc
removeWS (x:xs) full
    | isWhitespace x && isWhitespace (head full) = removeWS xs full
    | isWhitespace x = removeWS xs (' ':full)
    | otherwise = removeWS xs (x:full)
    
isWhitespace :: Char -> Bool
isWhitespace x = x == ' ' || x == '\t'