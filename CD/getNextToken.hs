import Text.Regex.Posix ((=~), MatchOffset, MatchLength)

data Types = Relop 
    | Arithmetic
    | Identifier
    | Keyword
    | BoolOp
    | LiteralString
    | Assignment
    | Number
    | Bitwise

data TokenStruct = Token {
    name   :: String
  , ofType :: Types
}

matchThing :: String -> String -> (MatchOffset, MatchLength)
matchThing str patt = str =~ patt :: (MatchOffset, MatchLength)
-- splitAt matchLength str gives the pair (matchedstring, rest)
-- then you can create token if fst pair is not empty and recurse on snd
numberPattern = "[+-]?([0-9]+[.])?[0-9]+"
arithmeticPattern = "[*+-/^%][=]?"
relopPattern = "((<|>)[=]*)|!=|=="
assignmentPattern = "="
boolPattern = "!|[|]{2}|&&"
bitPattern = "[|&]|<<|>>"
keywordPattern = "[_a-zA-Z][_a-zA-Z0-9]*"

matchStringLiteral :: String -> Int -> Int
  -- do not want to use regex for this
  -- return complete matched string literal or 
  -- throw error if not actually one

matchStringLiteral (x:y:rest) len
  | x == '\\' = matchStringLiteral rest len+2
  | y == '"' = len+2
  | otherwise = matchStringLiteral rest len+2

matchStringLiteral (x:xs) len | x == '"' = len+1
matchStringLiteral [] _ = error "malformed string"
matchStringLiteral _ _ = error "dunno man"

getNextToken:: String -> Either String TokenStruct
getNextToken "" = Left "empty string"