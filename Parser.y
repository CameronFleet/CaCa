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


%% 

Program : start Statements end { Program $2}

Statements : FromGet ';' as AsVars                       { FromGetExpr $1 $4}
           | FromGet ';' where '{' Equals '}' as AsVars  { FromGetWhere $1 $5 $8}

FromGet : from Relation get ToGet and FromGet            { FromGetAnd $2 $4 $6}   
        | from Relation get ToGet                        { FromGet $2 $4}

Equals : Var '=' Var ';'                                 { EqualVar $1 $3 }
       | Var '=' Var ';' Equals                          { EqualVars $1 $3 $5}

ToGet : Some                                             { Params $1}
      | Var                                              { Params1 $1}
      | ToGet ',' ToGet                                  { Params2 $1 $3}

Vars : Var                                               { Vars1 $1}
     | Var ',' Vars                                      { Vars2 $1 $3}

Some     : some string { Some $2 }
Var      : string      { Var $1 }
Relation : relation    { Relation $1}
    
{ 
parseError :: [Token] -> a
parseError _ = error "Parse error" 

data Program = Program Statements deriving Show

data Statements = FromGetExpr FromGet Vars
                | FromGetWhere FromGet Equals Vars deriving Show

data FromGet = FromGetAnd Relation ToGet FromGet
             | FromGet Relation ToGet deriving Show

data Equals = EqualVar Var Var 
            | EqualVars Var Var Equals deriving Show

data ToGet = Params Some 
           | Params1 Var 
           | Params2 ToGet ToGet deriving Show

data AsVars = AsVar Var 
            | AsVars Var Vars deriving Show

data Some = Some String deriving Show
data Var =  Var String deriving Show
data Relation = Relation String deriving Show

--main = do 
--  inStr <- getContents
--  let parseTree = caca (alexScanTokens inStr)  
--  putStrLn (show(parseTree))
}