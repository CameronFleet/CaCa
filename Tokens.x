{ 
module Tokens where 
}

%wrapper "basic" 
$digit = 0-9     
-- digits 
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
  some          { \s -> TokenSome}


  $digit+       ;
  $alpha+   { \s -> TokenString s } 
  $relation+   { \s -> TokenRelationalSymbol s } 

{ 
-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
  TokenStart       |
  TokenEnd         |
  TokenSemicolon   |
  TokenComma       |
  TokenFrom        |
  TokenGet         |
  TokenAnd         |
  TokenWhere       |
  TokenEquals      |
  TokenLCurlyBrace |
  TokenRCurlyBrace |
  TokenSome        |
  TokenString String  |
  TokenRelationalSymbol String 
  deriving (Eq,Show) 


--main = do 
--    smth <- getContents
--    print (alexScanTokens smth )

}