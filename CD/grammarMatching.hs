-- Grammar spec:
-- S-> aAcBe
-- A -> bA'
-- A' -> bA' | ε
-- B -> d
-- regex for the same would be "ab*cde"
main = do
    inp <- getLine 
    print . funcS $ inp

funcS str@(x:xs)|x == 'a' = case funcA . tail $ str of
            ('c' : rest) -> case funcB rest  of
                "e$" -> "string accepted"
                _ -> "not accepted"
            _ -> "not accepted"
funcS _ = "not accepted"

funcA (x:xs) | x == 'b' = funcA' xs
funcA _ = "not accepted"

funcA' (x:xs) | x == 'b' = funcA' xs
funcA' str = str

funcB (x:xs) | x == 'd' = xs
funcB _ = "not accepted"