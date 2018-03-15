
import Parser
import Tokens
import Control.Monad
import Data.Char
import Data.List
import System.Environment

{- 
TODO: 
- ERRORS 
  - PARSER
  - SEMANTIC
-}


-- The Data Type used to keep a record of every table, Each table holds the contents of exactly one relation 
type Tables = [(Relation, Table)] 

-- The Data Structure used to store all the information need about any given Relation
-- String   : Denotes the Variable or Existential quantification associated with a column
-- [String] : Denotes the Contents of a column, each String in this list are on distinct rows
{- EXAMPLE
A.csv = 1,2\n 3,4\n.; ∃k.A(x1,k)
Table = Columns "x1" ["1","3"] (Column "k" ["2","4"])
-}
data Table = Column String [String] | Columns String [String] Table deriving Show

-- The Enviroment system used to distinguish different Existential quantifiers in different enviroments
-- String : variable name of the existential quantifier
-- Tables : a list of all of the tables that exist within the enviroment 
{- EXAMPLE
A.csv = 1,2\n 3,4\n.; ∃k.A(x1,k)
Env = MonoEnv "k" (TablesEnv (Columns "x1" ["1","3"] (Column "k" ["2","4"])))
-}
data Env = PolyEnv Env Env | MonoEnv String Env | TablesEnv Tables deriving Show

-- The Data Identifier used to distinguish between Existential quantifers and variables
-- Note, each ExQ (Existential quantifer) also has a corrosponding Int generated uniquely by the enviroment it is in
data Basic = ExQ String Int | V String deriving Show

{- ===============================================================  EVAL  =============================================================== -}

-- {- OVERVIEW 

-- --eval              => -evaluates a Program AST and returns the Output String
-- --evalAsVars        => -evaluates AsVars and returns the OutputString

-- -}
-- Evaluates the AST (Program) using the 'Env' information retrieved from FromGet Statements, Returns the Output String
eval :: Program -> Env -> String
eval (Program stmnts asVars) env  = evalAsVars asVars (getEqualities stmnts) env

-- Evalutes the AsVars Statement
-- ERROR: Returns an error for the case where not all Variables declared are listed in the 'As' Statement
evalAsVars :: Vars -> [(String,String)] -> Env -> String
evalAsVars asVars equals env       | equalList freeVaribles listAsVars           = printAsVars (convertVars asVars) equals env
                                   | otherwise                                   = error errorMsg
                                   where freeVaribles = removeDuplicates (getFreeVariables env)
                                         listAsVars   = removeDuplicates (convertVars asVars)
                                         errorMsg     = "Undefined or Missing 'as' variables : DEFINED = "
                                                        ++ intercalate "," listAsVars ++ "   EXPECTED = "
                                                        ++ intercalate "," freeVaribles

{- ==========================================================  EvalAsVar AUX  ============================================================ -}

{- OVERVIEW 

-- printAsVars       => -returns output string gi

-- filterEqualVars   => -filters the list of 'rows' into a new list of 'rows' where equal vars have equal contents
-- filterExtQuant    => -filters the list of 'rows' into a new list of 'rows' where equal Ext. Quant. have equal contents
-- filterEqualities  => -filters the list of 'rows' into a new list of 'rows' where for each equality pair they have equal contents

-- processExtQuant   => -produces a list of 'rows' given an enviroment

-- removeExtQuant    => -processes the list of 'rows' removing the columns which show Ext. Quant. values 
-- orderAs           => -processes the list of 'rows' formating them in the format described in the As statement 

-- makeExtQuant      => -helper is a helper function of processExtQuant
-- getIdOfVar        => -helper of makeExtQuant

-}

-- Evaluates the AsVars returning the Output String, WITH EQUALITY! 
-- Parameter $1: List of asVars denoting the ordering; e.g. ["x1","x3","x2"] 
-- Parameter $2: List of equalities;  e.g. [("x1","x2")] meaning x1=x2
printAsVars :: [String] -> [(String,String)] -> Env -> String
printAsVars asVars equals env  = concat (map (orderAs asVars) (sort (outputRowsDupEqVars)))
                               where outputRows          = removeExtQuant (filterExtQuant (processExtQuant env))
                                     outputRowsEqVars    = (filterEqualities outputRows equals)
                                     outputRowsDupEqVars = (filterEqualVars outputRowsEqVars)

-- Returns an output of all of the 'rows' but those whose equal vars have equal contents. 
-- e.g. [(x1,2),(x2,4),(x1,2),(x1,3)] would be removed since the contents, 2 == 2 /= 3 of the equal Var x1. 
-- e.g. [(x1,2),(x2,4),(x1,2),(x7,3)] would be kept since the contentsm 2 == 2 of the equal Var x1
filterEqualVars :: [[(String,String)]] -> [[(String,String)]]
filterEqualVars combinations = [ c | c <- combinations, allVarsEqual c]

allVarsEqual :: [(String,String)] -> Bool 
allVarsEqual (_:[])          = True
allVarsEqual ((var,content):row) = (allVarsEqual' var content row) && (allVarsEqual row)

allVarsEqual' :: String -> String -> [(String,String)] -> Bool
allVarsEqual' var content ((var2,content2):[])  | var == var2 && content == content2 = True 
                                                | var /= var2                        = True
                                                | otherwise                          = False

allVarsEqual' var content ((var2,content2):row) | var == var2 && content == content2 = True && (allVarsEqual' var content row)
                                                | var /= var2                        = True && (allVarsEqual' var content row)
                                                | otherwise                          = False

-- Returns an output of all of the 'rows' but those whose equal EXT. Quantifers contents do not match are removed.
-- e.g. The Row [(VarBasic v, 3), (SomeBasic k, 2), (SomeBasic k, 3)] would be removed since the contents, 2 /= 3 of equal EXT. Quantifers k 
-- e.g. The Row [(VarBasic v, 3), (SomeBasic k, 2), (SomeBasic k, 2)] would be kept since the contents, 2 == 2 of the equal EXT. Quantifers k
filterExtQuant :: [[(Basic,String)]] -> [[(Basic, String)]]
filterExtQuant rows = [ row | row <- rows, allExtQuantEqual row ]

allExtQuantEqual :: [(Basic,String)]  -> Bool
allExtQuantEqual [] = True
allExtQuantEqual ((var,content):row) = allExtQuantEqual' var content row && allExtQuantEqual row

allExtQuantEqual' :: Basic -> String -> [(Basic,String)] -> Bool
allExtQuantEqual' _ _ [] = True
allExtQuantEqual' (V _) _ _ = True
allExtQuantEqual' (ExQ var id) content ((V _,_):row) = True && allExtQuantEqual' (ExQ var id) content row
allExtQuantEqual' (ExQ var id) content ((ExQ var2 id2, content2):row) | var == var2 && id == id2 && content == content2 = True && allExtQuantEqual' (ExQ var id) content row
                                                                       | var == var2 && id == id2 && content /= content2 = False
                                                                       | otherwise    
                                                                                                          = True && allExtQuantEqual' (ExQ var id) content row
-- Evaluates each row for all equalities, checking the contents of variables that are meant to be equal and returning those whose content do match
-- Parameter $1: List of every row, a single row can be described as such: [(x1,"content"), (x2, "content"), (x3,"content")]
-- Parameter $2: List of all equalities required ; e.g. [("x1","x2"),("x3","x4")] means x1=x2 and x3=x4 
filterEqualities :: [[(String,String)]] -> [(String,String)] -> [[(String,String)]]
filterEqualities rows [] = rows 
filterEqualities rows (eq:[])  = rowsEqVars rows eq
filterEqualities rows (eq:eqs) = filterEqualities (rowsEqVars rows eq) eqs 

rowsEqVars :: [[(String,String)]] -> (String, String) -> [[(String, String)]]
rowsEqVars rows equal = [r | r <- rows, eqVars r equal]

eqVars :: [(String,String)] -> (String,String) -> Bool
eqVars rows (eqVar1, eqVar2) | equalList contentVar1 contentVar2 = True
                             | otherwise = False
                              where contentVar1 = removeDuplicates (eqVars' rows eqVar1)
                                    contentVar2 = removeDuplicates (eqVars' rows eqVar2)

eqVars' :: [(String,String)] -> String -> [String]
eqVars' ((var,content):[]) eqVar   | var == eqVar = [content]
                                   | otherwise    = []

eqVars' ((var,content):rows) eqVar | var == eqVar = [content] ++ eqVars' rows eqVar
                                   | otherwise    = eqVars' rows eqVar

-- Removes all of the existing Ext.Quantifers from the output string, also turns Basic -> String since all will be vars! 
-- Parameter $1: List of rows with ExQ indentifiers 
removeExtQuant :: [[(Basic, String)]] -> [[(String,String)]]
removeExtQuant []       = []
removeExtQuant (r:rows) = [removeExtQuant' r] ++ removeExtQuant rows 

removeExtQuant' :: [(Basic, String)] -> [(String,String)]
removeExtQuant' []                     = []
removeExtQuant' ((ExQ _ _, _):row)      = removeExtQuant' row
removeExtQuant' ((V var, content):row) = [(var,content)] ++ removeExtQuant' row

-- Parameter $1: all Rows in form (Variable, Content) so e.g if r1 x1, (Row 1, Column x1) contains "Hello" then this will be [("x1", "Hello")]
-- Parameter $2: the AsVar variables e.g in the form such ["x1","x4","x2","x3"] denotes the printing out in this order                                 
orderAs :: [String] -> [(String, String)] -> String 
orderAs (asVar:[]) row = orderAs' row asVar ++ "\n"
orderAs (asVar:asVars) row = (orderAs' row asVar) ++ "," ++ orderAs asVars row

orderAs' :: [(String, String)] -> String -> String
orderAs' []  _ = error "smth went wrong"
orderAs' ((var,content):row) v  | v == var = content
                                | otherwise = orderAs' row v

-- ==========================================================  ENV MANIPULATORS  ====================================================================================

{- OVERVIEW 

-- getFreeVariables  => -returns all the freeVariables in a given Env

-- processExtQuant   => -produces a list of 'rows' given an enviroment

-- makeExtQuant      => -helper is a helper function of processExtQuant
-- getIdOfVar        => -helper of makeExtQuant

-}

getFreeVariables :: Env -> [String]
getFreeVariables (TablesEnv tables) = removeDuplicates (getTablesVars tables)
getFreeVariables (MonoEnv v env) = (getFreeVariables env) \\ [v]
getFreeVariables (PolyEnv env1 env2) = removeDuplicates ((getFreeVariables env1) ++ (getFreeVariables env2))


-- Fully Evaluates an Enviroment providing a breakdown of rows retaining the nature of unique ext. quantifers
-- Parameter $1: An enviroment
-- Output: Rows in the structure [ [(ExQ k 100, "hello"),(V x1, "bye")] , [(ExQ k 100, "hi"),(V x1, "bibi")] ] where there are two rows, and two columns
processExtQuant :: Env -> [[(Basic,String)]]
processExtQuant env = combineTables (processExtQuant' env)

processExtQuant' :: Env -> [[(Basic,[String])]] 
processExtQuant' (TablesEnv tables)  = makeExtQuantList' [] (tablesToVarList tables) 1000
processExtQuant' (MonoEnv v env)     = makeExtQuantList [(v,0)] env 0
processExtQuant' (PolyEnv env1 env2) = processExtQuant'' env1 1 ++ processExtQuant'' env2 100

processExtQuant'' :: Env -> Int -> [[(Basic,[String])]] 
processExtQuant'' (TablesEnv tables) id  = makeExtQuantList' [] (tablesToVarList tables) id 
processExtQuant'' (MonoEnv v env) id     = makeExtQuantList [(v,id)] env id
processExtQuant'' (PolyEnv env1 env2) id = processExtQuant'' env1 (id+1) ++ processExtQuant'' env2 (id+100)

-- Make the correct output list of columns
-- Parameter $1: Currently boundVars by the current Env
-- Parameter $2: The current ENV 
-- Parameter $3: The current accumalated ID
-- Output: A list of the columns in each table in the format [[(ExQ k 100,["hello","hi"]),(V x1,["bye","bibi"])]] each item is a individual relation(table) 
makeExtQuantList :: [(String,Int)] -> Env -> Int -> [[(Basic,[String])]]
makeExtQuantList boundVars (TablesEnv tables) id  = makeExtQuantList' boundVars (tablesToVarList tables) id
makeExtQuantList boundVars (MonoEnv v env) id     = makeExtQuantList (boundVars ++ [(v,id)]) env id
makeExtQuantList boundVars (PolyEnv env1 env2) id = (makeExtQuantList boundVars env1 (id+1)) ++ (makeExtQuantList boundVars env2 (id+100))

makeExtQuantList' :: [(String,Int)] -> [[(String,[String])]] -> Int -> [[(Basic,[String])]]
makeExtQuantList' _ [] _ = []
makeExtQuantList' boundVars (r:rows) id = [(makeExtQuantList'' boundVars r id)] ++ (makeExtQuantList' boundVars rows id)

makeExtQuantList'' :: [(String,Int)] -> [(String,[String])] -> Int -> [(Basic,[String])]
makeExtQuantList'' boundVars ((var, content):[]) id  | var `elem` (map fst boundVars) = [(ExQ var (getIdOfVar var boundVars), content)] 
                                                     | otherwise                      = [(V var, content)] 

makeExtQuantList'' boundVars ((var, content):row) id | var `elem` (map fst boundVars) = [(ExQ var (getIdOfVar var boundVars), content)] ++ makeExtQuantList'' boundVars row id
                                                     | otherwise                      = [(V var, content)] ++ makeExtQuantList'' boundVars row id

-- Return the id of a given bound var
getIdOfVar :: String -> [(String,Int)] -> Int
getIdOfVars var1 ((var2,id):[])   | var1 == var2 = id 
                                  | otherwise = error "smth"
getIdOfVar var1 ((var2,id):bVars) | var1 == var2 = id
                                  | otherwise  = getIdOfVar var1 bVars


-- ==========================================================  TABLE MANIPULATORS  ====================================================================================

{- OVERVIEW

-- getTablesVars      => -returns a list of all variables in all tables in tables
-- getRow             => -returns a specified row by index in a given table
-- getNumberOfRows    => -returns a Int value of how many rows are in a given table

-- rowsInTable        => -returns a Int value of how many rows there are in a given table in format [(Column variable, [contents of column])]
-- getRowInList       => -returns a specified row by index in a given table in format [(Column variable, [contents of column])]


-- areDuplicateVars   => -decides if a given list of tables contains multiple of the same variables, returning bool

-- getNextAndCombine  => -turns a list of tables into a list of rows containing all combinations
-- combineTables      => -turns a list of tables in the form [[(Column variable, [contents of column])]] into a list of rows containing all combinations
-- convertTable       => -turns a single table in the form [(Column variable, [contents of column])] into the list of rows containing all combinations
-- tablesToVarList    => -turns a list of tables into a list of all variables present, the form [[(Column variable, [contents of column])]]
-}

-- Returns a list of all variables in all tables given as an arguement
-- e.g. ( (Relation "A", (Column "x1" [])), (Relation "B", (Column "x2" [])) ) ===> ["x1","x2"]
getTablesVars :: Tables -> [String]
getTablesVars ((_, table):[])     = getTableVars table
getTablesVars ((_, table):tables) = (getTableVars table) ++ (getTablesVars tables)

getTableVars :: Table -> [String]
getTableVars (Column var _)        = [var]
getTableVars (Columns var _ table) = [var] ++ (getTableVars table)

-- Returns the Row of a given index in a table in form [(column, content)]
-- e.g. (Columns x1 ["c","cc","ccc","cccc"] (Column x2 ["s","sa","sas","sasa"])) 4 ===> [(VarBasic x1,"cccc"),(VarBasic x2, "sasa")]
getRow :: Table -> Int -> [(String, String)]
getRow (Column v cs) index        = [(v,cs!!index)]
getRow (Columns v cs table) index = [(v,cs!!index)] ++ (getRow table index)

-- Returns the number of rows in a given table
-- e.g. (Column _ ["s","sa","sas","sasa"])  ===> 4
getNumberOfRows :: Table -> Int
getNumberOfRows (Column v [])            = 0
getNumberOfRows (Column v (c:cs))        = 1 + getNumberOfRows (Column v cs)
getNumberOfRows (Columns v [] table)     = 0
getNumberOfRows (Columns v (c:cs) table) = 1 + getNumberOfRows (Columns v (cs) table)


-- Decides if a list of tables contains any duplicate variables 
-- e.g. ( (Relation "A", (Column "x1" [])), (Relation "B", (Column "x2" [])) ) ===> False
-- e.g. ( (Relation "A", (Column "x1" [])), (Relation "B", (Column "x1" [])) ) ===> True
areDuplicateVars :: Tables -> Bool
areDuplicateVars tables | duplicates (getTablesVars tables) = True
                        | otherwise = False

getTables :: Tables -> [Relation] -> Tables
getTables [] _ = []
getTables ((r,table):tables) relationsToGet | r `elem` relationsToGet = [(r,table)] ++ (getTables tables relationsToGet)
                                            | otherwise               = (getTables tables relationsToGet)


-- Combines all individual Tables in Tables to produce one coherent row across all relations
-- e.g. 
{- Relation A = 1,2\n 3,4\n ;   Relation B = 5,6\n 7,8\n ;
   Tables ts = [(Relation A, tableA), (Relation B, tableB)]

   Will first combine [[]] with tableA, producing        -> [ [(VarBasic _, 1),(VarBasic _, 2)], [(VarBasic _, 3), (VarBasic _, 4)] ]
   Hence we can see each list within the list is a 'Row' 

   We will now combine this above with tableB, producing -> [ [(VarBasic _, 1),(VarBasic _,2),(VarBasic _,5), (VarBasic _,6)],
                                                              [(VarBasic _, 1),(VarBasic _,2),(VarBasic _,7), (VarBasic _,8)], 
                                                              [(VarBasic _, 3),(VarBasic _,4),(VarBasic _,5), (VarBasic _,6)],
                                                              [(VarBasic _, 3),(VarBasic _,4),(VarBasic _,7), (VarBasic _,8)] ]
-}
getNextAndCombine :: [[(String,String)]] -> Tables -> [[(String,String)]]
getNextAndCombine combinations ((_,table):[])     = [ c ++ getRow table x | c <- combinations, x <-[0..(getNumberOfRows table -1)]]
getNextAndCombine combinations ((_,table):tables) = getNextAndCombine ([c ++ getRow table x | c <- combinations, x <-[0..(getNumberOfRows table -1)]]) tables


combineTables :: [[(Basic,[String])]] -> [[(Basic,String)]]
combineTables []             = [] 
combineTables (table:[])     = convertTable table
combineTables (table:tables) = combineTables' tables (convertTable table) 

combineTables' :: [[(Basic,[String])]] -> [[(Basic,String)]] -> [[(Basic,String)]]
combineTables' (newTable:[]) table     = [ row ++ newRow | row <- table, newRow <- (convertTable newTable)]
combineTables' (newTable:tables) table = combineTables' tables ([ row ++ newRow | row <- table, newRow <- (convertTable newTable)])

convertTable :: [(Basic,[String])] -> [[(Basic, String)]]
convertTable table = [getRowInList table index | index <- [0..rowsInTable table -1]]

rowsInTable :: [(Basic,[String])] -> Int
rowsInTable ((_,xs):_) = length xs

getRowInList :: [(Basic,[String])] -> Int -> [(Basic, String)]
getRowInList [] _ = []
getRowInList ((var,content):table) index = [(var, content!!index)] ++ getRowInList table index

tablesToVarList :: Tables -> [[(String,[String])]]
tablesToVarList [] = []
tablesToVarList ((_,table):tables) = [tablesToVarList' table] ++ tablesToVarList tables

tablesToVarList' :: Table -> [(String,[String])]
tablesToVarList' (Column var content) = [(var,content)]
tablesToVarList' (Columns var content table) = [(var,content)] ++ tablesToVarList' table

-- =============================================================  ENV MAKERS  =======================================================================================

{- OVERVIEW

-- getFilePaths  => -returns a list of all Relation Symbol filepaths given a Program AST 
-- getVars       => -returns a list of typles of all Relation Symbols along with a list all of the Variables/Ext. Quant within this relation given a Program AST 

-- makeTables    => -makes a list of Tables, given a list of the content from each Relation and the associated list made by getVars 
-- makeEnv       => -makes an Env given a list of tables and a program
-}

-- Returns FILEPATHS from a program, in the format ["A.csv", "B.csv"] 
getFilePaths :: Program -> [FilePath]
getFilePaths (Program statement _ ) = getFilePaths' statement

getFilePaths' :: Statements -> [FilePath]
getFilePaths' (FromGetExpr fromGet)              = getFilePaths'' fromGet
getFilePaths' (FromGetWhere fromGet _)           = getFilePaths'' fromGet
getFilePaths' (AnyExpr _ statement1 statement2 ) = getFilePaths' statement1 ++ getFilePaths' statement2
getFilePaths' (Any _ statement)                  = getFilePaths' statement 

getFilePaths'' :: FromGet -> [FilePath]
getFilePaths'' (FromGetAnd (Relation r) _ fromGet)   = [filepath r] ++ getFilePaths'' fromGet
getFilePaths'' (FromGet (Relation r) _)              = [filepath r]

-- Get Relations Variables!, Each variable is assigned to a column in the table! 
-- Takes a AST, and returns [(Relation "A", ["x1","x2","k","x3"]), (Relation "B" , ["x1","x2","k","x3"])] 
getVars :: Program -> [(Relation, [String])]
getVars (Program statement _)    = getVars' statement

getVars' :: Statements -> [(Relation, [String])]
getVars' (FromGetExpr fromGet)              = getVars'' fromGet
getVars' (FromGetWhere fromGet _)           = getVars'' fromGet
getVars' (AnyExpr _ statement1 statement2 ) = getVars' statement1 ++ getVars' statement2
getVars' (Any _ statement)                  = getVars' statement 


getVars'' :: FromGet -> [(Relation, [String])] 
getVars'' (FromGetAnd relation vars fromGet) = [(relation, (getVars''' vars))] ++ getVars'' fromGet
getVars'' (FromGet relation vars)            = [(relation, (getVars''' vars))]

getVars''' :: Vars -> [String]
getVars''' (Param (Var v))       = [v]
getVars''' (Params (Var v) vars) = [v] ++ getVars''' vars

-- Generate the Tables. 
-- Parameter $1: List of each Relation data in order e.g ["hi,bye", "low,high"]
-- Parameter $2: Output of GetVars, the Relation, in order, with the String assignments
makeTables :: [String] -> [(Relation, [String])] -> Tables 
makeTables (content:[]) ((relation, vars):[])       = [(relation, (makeTable content vars))]
makeTables (content:contents) ((relation, vars):ys) = [(relation, (makeTable content vars))] ++ (makeTables contents ys)
makeTables _ _                                      = error "There should be an error here"

makeTable :: String -> [String] -> Table
makeTable content basics = makeTable' (splitContents content) basics

makeTable' :: [[String]] -> [String] -> Table
makeTable' [] (v:[])              = Column v []
makeTable' [] (v:vars)            = Columns v [] (makeTable' [] vars)
makeTable' (c:[]) (v:[])          = Column v c
makeTable' (c:content) (v:vars)   = Columns v c (makeTable' content vars)
makeTable' _ _                    = error "There should be an error here2"

-- Generates the Enviroment
makeEnv :: Tables -> Program -> Env
makeEnv tables (Program (FromGetExpr _) _)                       = TablesEnv tables
makeEnv tables (Program (FromGetWhere _ _) _)                    = TablesEnv tables
makeEnv tables (Program (Any (Var v) nestedStmnts) _)            = MonoEnv v (makeEnv' tables nestedStmnts )
makeEnv tables (Program (AnyExpr (Var v) nestedStmnts stmnts) _) = PolyEnv (MonoEnv v (makeEnv' tables nestedStmnts)) (makeEnv' tables stmnts)

makeEnv' :: Tables -> Statements -> Env
makeEnv' tables (FromGetExpr fromGet)                 = makeEnv'' tables fromGet
makeEnv' tables (FromGetWhere fromGet _)              = makeEnv'' tables fromGet
makeEnv' tables (Any (Var v) nestedStmnts)            = MonoEnv v (makeEnv' tables nestedStmnts )
makeEnv' tables (AnyExpr (Var v) nestedStmnts stmnts) = PolyEnv (MonoEnv v (makeEnv' tables nestedStmnts )) (makeEnv' tables stmnts)

makeEnv'' :: Tables -> FromGet -> Env
makeEnv'' tables (FromGet r _)               = TablesEnv (getTables tables [r])
makeEnv'' tables (FromGetAnd r vars fromGet) = TablesEnv (getTables tables (getRelations (FromGetAnd r vars fromGet)))

-- ================================================================  AUX  =============================================================================================

{- OVERVIEW

-- convertVars      => -converts of type Vars into [String] containing all the vars 
-- convertEquals    => -converts of type Equals into [(String,String)] containg all the equalities, e.g. (x1,x2) x1=x2; 
 
-- getRelations     => -converts of type FromGet into [Relation] containg all relations present 
-- getEqualities    => -converts of type Statement into a complete list of all equalities in the program
-- getStringVar     => -converts of type Var into a String 

-- equalList        => -decides if two lists are equal
-- duplicates       => -decides if a list contains a duplicate

-- removeDuplicates => -produces a list, given a list, without duplicates

-- splitContents    => -splits a single row string and splits it based on CSV rules producing [String]
-- clean            => -splits a single row on newline basis

-- wordsWhen        => -helper for clean and splitContents
-- removeWhiteSpace => -helper for original input, removes all whiteSpace 

-}


-- Turns the Data Type:  Vars ====> [String] ; Retains order
convertVars :: Vars -> [String]
convertVars (Param (Var s))       = [s]
convertVars (Params (Var s) vars) = [s] ++ (convertVars vars)

-- converts Equals into a [(String,String)] such that [("x1","x2"),("x3","x4")] x1=x2 and x3=x4
convertEquals :: Equals -> [(String,String)]
convertEquals (EqualVar var1 var2)     = [(getStringVar var1, getStringVar var2)] 
convertEquals (EqualVars var1 var2 eq) = [(getStringVar var1, getStringVar var2)] ++ (convertEquals eq) 


getRelations :: FromGet -> [Relation]
getRelations (FromGet r _)            = [r]
getRelations (FromGetAnd r _ fromGet) = [r] ++ getRelations fromGet


getEqualities :: Statements -> [(String,String)]
getEqualities (FromGetExpr fromGet)             = []
getEqualities (FromGetWhere fromGet equals)     = convertEquals equals
getEqualities (Any _ stmnt)                     = getEqualities stmnt
getEqualities (AnyExpr _ stmnt1 stmnt2)         = getEqualities stmnt1 ++ getEqualities stmnt2

-- takes the sting part of the Var: Var "x1" = "x1"
getStringVar :: Var -> String
getStringVar (Var v) = v

equalList :: [String] -> [String] -> Bool
equalList x y = null (x \\ y) && null (y \\ x)

removeDuplicates :: [String]  -> [String]
removeDuplicates [] = []
removeDuplicates (x:xs)  | x `elem` xs  = removeDuplicates xs 
                         | otherwise    = x:(removeDuplicates xs ) 


-- if anything is true => theres is a duplicate
duplicates :: (Eq a) => [a] -> Bool
duplicates []     = False
duplicates (x:xs) = x `elem` xs || duplicates xs

-- "hi,bye" to [["hi"], ["bye"]]; "hi,bye\n zdraveyte,chao" to [["hi","zdraveyte"],["bye", "chao"]]
splitContents :: String -> [[String]]
splitContents s | length s == 0 = []
                | s == "\n" = []
                | otherwise = splitContents'' (splitContents' (wordsWhen (=='\n') s))

splitContents' :: [String] -> [[String]]
splitContents' (s:[]) = [(wordsWhen (==',') s)]
splitContents' (s:ss) = [(wordsWhen (==',') s)] ++ splitContents' ss

splitContents'' :: [[String]] -> [[String]]
splitContents'' ([]:xs) = []
splitContents'' ss      = [map head ss] ++ splitContents'' (map tail ss)

-- Takes a list of CSV e.g : ["Bob", "Alice", "John\n"]  ====> ["Bob", "Alice", "John"]
clean :: [String] -> [String]
clean (x:[]) = wordsWhen (=='\n') x
clean (x:xs) = wordsWhen (=='\n') x ++ clean xs

-- Acts like a SplitOn Function  , wordsWhen (==',') "This,Is,A,CSV" =====> ["This", "Is", "A" , "CSV"]
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

-- Removes all white space from a list of strings
removeWhiteSpace :: [String] -> [String]
removeWhiteSpace list = [ filter (/=' ') l | l <- list]

-- ================================================================  IO  =============================================================================================

{- OVERVIEW

--readFiles  => -takes a list of FilePaths and reads from each file path producing a list with the contents of each file inside
--filepath   => -takes a string and converts it into a FilePath, with .csv

-}

readFiles :: [FilePath] -> IO [String]
readFiles ss = mapM readFile ss 

filepath :: String -> FilePath
filepath s = (s ++ ".csv")

-- ================================================================  MAIN  =========================================================================================== 
main :: IO ()
main = do

    -- Reads : arguemnts from command line, the first arguement is the programFile which is read
    args <- getArgs
    program <- readFile (head args)

    -- Assigns : ast, to the abstract syntax tree generated by lexer and parser
    let ast = caca (alexScanTokens (program))

    -- Assigns : relationContents, to a list containing all of the files info, e.g. file A contains : "hi,bye" and file B contanis "low,high"
    -- then relationContents will be = ["hi,bye", "low,high"]
    -- getFilePaths         ; Will navigate the ast to find any and all declarations of: from A, from B and produce [FilePath]: ["A.csv", "B.csv"]
    relationContents <- readFiles (getFilePaths ast)

    let content = removeWhiteSpace relationContents

    -- Assigns : tables, to be the table containing all info needed to parse correctly. e.g if the files above are name with variable names
    --  A(x1,x2) and B(x3,x4) then Tables will be the following
    -- [(Relation "A", Columns x1 ["hi"] (Column x2 ["bye"])), (Relation "B", Columns x3 ["low"] (Column x4 ["high"]))]
    -- makeTables.          ; Should generate of type Tables. 
    -- getVars.             ; Must include to which Relation each variable is related to, so maybe [(Relation "A", ["x1","x2"]), (Relation "B", ["x3","x4"])]
    let tables = makeTables content (getVars ast)

    -- Assigns : env, to be the enviroment containing all the info needed to parse correctly,
    let env = makeEnv tables ast

    -- Prints : the full evaluation, eval ast tables for any given program defined by our BNF should produce the desired result with proper error handling.
    putStr (eval ast env)

