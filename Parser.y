{ 
module Parser where 
import Tokens 


}

%name caca 
%tokentype { Token } 
%token 
    start   { TokenStart _} 
    end     { TokenEnd _} 
    '='     { TokenEquals _} 
    '{'     { TokenLCurlyBrace _} 
    '}'     { TokenRCurlyBrace _} 
    where   { TokenWhere _} 
    ';'     { TokenSemicolon _}
    and     { TokenAnd _}
    ','     { TokenComma _}
    from    { TokenFrom _}
    get     { TokenGet _}
    string  { TokenString _ $$ }
    relation { TokenRelationalSymbol _ $$ }
    as      { TokenAs _}
    any     { TokenAny _}
    '('     { TokenLBracket _}
    ')'     { TokenRBracket _}


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


happyError :: [Token] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\n")
  where
  lcn =   case tks of
      [] -> "end of file"
      (tk:_) -> "line " ++ show l ++ ", column " ++ show c
                where
                     AlexPn _ l c = token_posn tk

--main = do 
--  inStr <- getContents
--  let parseTree = caca (alexScanTokens inStr)  
--  putStrLn (show(parseTree))
}