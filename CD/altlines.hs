import System.Environment (getArgs)
import System.Directory.Internal.Prelude (exitFailure)
main :: IO ()
main = do
  args <- getArgs
  if length args < 3 then exitFailure else altlines args

altlines :: [FilePath] -> IO ()
altlines (x:y:yy:xs) = do
  a <- readFile x
  let aa = lines a
  b <- readFile y
  let bb = lines b
  let lena = length aa
  let lenb = length bb
  let cond = lena < lenb
  let cond = lena < lenb
  let z = if cond
              then zip (aa ++ replicate (lenb-lena) "") bb
              else zip aa (bb ++ replicate (lena-lenb) "")
  let res = map (\(m,n) -> if m == "" || n == "" then m ++ n else m ++ "\n" ++ n) z
  writeFile yy (unlines res)

altlines xs = do
  return ()