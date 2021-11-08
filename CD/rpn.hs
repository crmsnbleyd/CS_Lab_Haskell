main :: IO ()
main = do
    inp <- getLine
    print . rpn $ inp

rpn :: String -> Float
rpn str = helper (words str) []

helper :: [String] -> [String] -> Float
helper [] [x] = read x :: Float
helper [] _ = error "malformed expression"

helper (op:rest) (x:y:stack)
    | op == "+" = helper rest (show (a+b): stack)
    | op == "-" = helper rest (show (a-b): stack)
    | op == "*" = helper rest (show (a*b): stack)
    | op == "/" = helper rest (show (a/b): stack)
    | op == "^" = helper rest (show (a**b): stack)
    where a = read x :: Float
          b = read y :: Float

helper (num:rest) stack= helper rest (num:stack)