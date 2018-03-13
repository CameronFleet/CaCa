{ 
module Parser where 
import Tokens 


}

%name caca 
%tokentype { Token } 
%error { parseError }
%token 
    start   { TokenStart } 
    end     { TokenEnd } 
    '='     { TokenEquals } 
    '{'     { TokenLCurlyBrace } 
    '}'     { TokenRCurlyBrace} 
    some    { TokenSome } 
    where   { TokenWhere } 
    ';'     { TokenSemicolon }
    and     { TokenAnd }
    ','     { TokenComma }
    from    { TokenFrom }
    get     { TokenGet }
    string  { TokenString $$ }
    relation { TokenRelationalSymbol $$ }
    as      { TokenAs}
    any     { TokenAny}
    '('     { TokenLBracket }
    ')'     { TokenRBracket }


%% 

Program : start Statements as Vars end        { Program $2 $4}

Statements : FromGet                                        { FromGetExpr $1}
           | FromGet where '{' Equals '}'                   { FromGetWhere $1 $4 }
           | any '(' Var ')' '{' Statements '}' Statements  { AnyExpr $3 $6 $8}
           | Statements any '(' Var ')' '{' Statements '}'  { AnyExpr $4 $7 $1}  
           | any '(' Var ')' '{' Statements '}'             { Any $3 $6}            

FromGet : from Relation get Vars and FromGet      { FromGetAnd $2 $4 $6}   
        | from Relation get Vars                  { FromGet $2 $4}

Equals : Var '=' Var ';'                          { EqualVar $1 $3 }
       | Var '=' Var ';' Equals                   { EqualVars $1 $3 $5}

Vars    :  Var                                    { Param $1}
        |  Var ',' Vars                           { Params $1 $3}    

Var      : string      { Var $1 }
Relation : relation    { Relation $1}
    
{ 
parseError :: [Token] -> a
parseError _ = error "Parse error on " 

data Program = Program Statements Vars deriving Show

data Statements = FromGetExpr FromGet 
                | FromGetWhere FromGet Equals  
                | AnyExpr Var Statements Statements 
                | Any Var Statements deriving Show

data FromGet = FromGetAnd Relation Vars FromGet
             | FromGet Relation Vars deriving Show

data Equals = EqualVar Var Var 
            | EqualVars Var Var Equals deriving Show

data Vars = Param Var 
          | Params Var Vars deriving Show

data Var =  Var String deriving (Show,Eq)
data Relation = Relation String deriving (Show,Eq)


--main = do 
--  inStr <- getContents
--  let parseTree = caca (alexScanTokens inStr)  
--  putStrLn (show(parseTree))
}