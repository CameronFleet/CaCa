{ 
module Tokens where 
}

%wrapper "basic"     
$alpha = [a-z0-9]    
$relation = [A-Z]
-- alphabetic characters

tokens :-
$white+       ; 
  "//".*        ; 
  start         { \s -> TokenStart } 
  end           { \s -> TokenEnd }
  \;            { \s -> TokenSemicolon }
  \,            { \s -> TokenComma }
  from          { \s -> TokenFrom }
  get           { \s -> TokenGet }
  and           { \s -> TokenAnd }
  where         { \s -> TokenWhere }
  \=            { \s -> TokenEquals }
  \{            { \s -> TokenLCurlyBrace }
  \}            { \s -> TokenRCurlyBrace }
  as            { \s -> TokenAs}
  some          { \s -> TokenSome}
  any           { \s -> TokenAny}
  \(            { \s -> TokenLBracket }
  \)            { \s -> TokenRBracket }



  $alpha+      { \s -> TokenString s } 
  $relation+   { \s -> TokenRelationalSymbol s } 

{ 
-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
  TokenStart          |
  TokenEnd            |
  TokenSemicolon      |
  TokenComma          |
  TokenFrom           |
  TokenGet            |
  TokenAnd            |
  TokenWhere          |
  TokenEquals         |
  TokenLCurlyBrace    |
  TokenRCurlyBrace    |
  TokenSome           |
  TokenAs             |
  TokenAny            |
  TokenLBracket       |
  TokenRBracket       |
  TokenString String  |
  TokenRelationalSymbol String 
  deriving (Eq,Show) 


--main = do 
--    smth <- getContents
--    print (alexScanTokens smth )

}