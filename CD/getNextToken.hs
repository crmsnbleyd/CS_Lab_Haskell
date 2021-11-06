data Types = Relop 
    | Arithmetic
    | Identifier
    | Keyword
    | BoolOp
    | LiteralString
    | Assignment

data TokenStruct = Token {
    name   :: String
  , ofType :: Types
}

getNextToken:: String -> TokenStruct