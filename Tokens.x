{ 
module Tokens (Token(..), AlexPosn(..), alexScanTokens, token_posn) where 
}

%wrapper "posn"     

$alpha = [a-z0-9]    
$relation = [A-Z]

tokens :-
  $white+       ; 
  "//".*        ; 
  start         { tok (\p s -> TokenStart p)} 
  end           { tok (\p s -> TokenEnd p) }
  \;            { tok (\p s -> TokenSemicolon p) }
  \,            { tok (\p s -> TokenComma p) }
  from          { tok (\p s -> TokenFrom p) }
  get           { tok (\p s -> TokenGet p) }
  and           { tok (\p s -> TokenAnd p) }
  where         { tok (\p s -> TokenWhere p) }
  \=            { tok (\p s -> TokenEquals p) }
  \{            { tok (\p s -> TokenLCurlyBrace p) }
  \}            { tok (\p s -> TokenRCurlyBrace p) }
  as            { tok (\p s -> TokenAs p) }
  any           { tok (\p s -> TokenAny p) }
  \(            { tok (\p s -> TokenLBracket p) }
  \)            { tok (\p s -> TokenRBracket p) }



  $alpha+      { tok (\p s -> TokenString p s) } 
  $relation+   { tok (\p s -> TokenRelationalSymbol p s) } 

{ 

tok f p s = f p s

-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
  TokenStart AlexPosn         |
  TokenEnd AlexPosn           |
  TokenSemicolon AlexPosn     |
  TokenComma AlexPosn         |
  TokenFrom  AlexPosn         |
  TokenGet  AlexPosn          |
  TokenAnd  AlexPosn          |
  TokenWhere  AlexPosn        |
  TokenEquals  AlexPosn       |
  TokenLCurlyBrace  AlexPosn  |
  TokenRCurlyBrace  AlexPosn  |
  TokenAs       AlexPosn      |
  TokenAny      AlexPosn      |
  TokenLBracket    AlexPosn   |
  TokenRBracket    AlexPosn   |
  TokenString AlexPosn String |
  TokenRelationalSymbol AlexPosn String 
  deriving (Eq,Show) 

token_posn (TokenStart p) = p
token_posn (TokenEnd p) = p
token_posn (TokenComma p) = p
token_posn (TokenFrom p) = p
token_posn (TokenGet p) = p
token_posn (TokenSemicolon p) = p
token_posn (TokenAnd p) = p
token_posn (TokenWhere p) = p
token_posn (TokenEquals p) = p
token_posn (TokenLCurlyBrace p) = p
token_posn (TokenRCurlyBrace p) = p
token_posn (TokenAs p) = p
token_posn (TokenAny p) = p
token_posn (TokenLBracket p) = p
token_posn (TokenRBracket p) = p
token_posn (TokenString p _) = p
token_posn (TokenRelationalSymbol p _) = p


--main = do 
--    smth <- getContents
--    print (alexScanTokens smth )

}