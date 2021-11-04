import System.Environment (getArgs)
import System.Exit (exitFailure)
main :: IO ()
main = do
    args <- getArgs
    if length args < 2 then exitFailure 
    else 
        func args
    where func (x:y:xs) = do
            inp <- readFile x
            writeFile y inp
          func _ = do
                return ()