
import Parser
import Tokens
import Control.Monad
import Data.Char
import Data.List


{- 
TODO: 
- sorting
- errors
- testing
- syntax
- removing white space - maybe done, tested
-}


-- The Data Type used to keep a record of every table, Each table holds the contents of exactly one relation 
type Tables = [(Relation, Table)] 

-- The Data Structure used to store all the information need about any given Relation
-- Basic    : Denotes the Variable or Existential quantification associated with a column
-- [String] : Denotes the Contents of a column, each String in this list are on distinct rows
{- EXAMPLE
A.csv = 1,2\n 3,4\n.; ∃k.A(x1,k)
Table = Columns (VarBasic "x1") ["1","3"] (Column (SomeBasic "k") ["2","4"])
-}
data Table = Column String [String] | Columns String [String] Table deriving Show

-- Comment this
data Env = PolyEnv Env Env | MonoEnv String Env | TablesEnv Tables 

{- ===============================================================  EVAL  =============================================================== -}

{- OVERVIEW 

--eval              => -evaluates a Program AST and returns the Output String
--evalFromGetExpr   => -evaluates AsVars and returns the Output String
--evalFromGetWhere  => -evaluates Equals and AsVars and returns the Output String
--evalAsVars        => -evaluates AsVars and returns the OutputString
--evalAsVars        => -evaluates Equals and AsVars and returns the Output String

-}

-- Evaluates the AST (Program) using the 'Tables' information retrieved from FromGet Statements, Returns the Output String
eval :: Program -> Tables -> Env -> String
eval (Program statement _) tables env = evalStatements statement tables env

evalStatements :: Statements -> Tables -> Env -> String
evalStatements 

-- Evaluates the FromGetExpr, returning the Output String
evalFromGetExpr :: FromGet -> AsVars -> Tables -> String
evalFromGetExpr _ asVars tables = evalAsVars asVars tables

-- Evaluates the FromGetWhere, returning the Output String
evalFromGetWhere :: FromGet -> Equals -> AsVars -> Tables -> String
evalFromGetWhere _ equals asVars tables = evalAsVars' asVars equals tables

-- Evaluates the AsVars for a FromGetExpr
-- ERROR: Returns an error for the case where not all Variables declared are listed in the 'As' Statement
evalAsVars :: AsVars -> Tables -> String
evalAsVars asVars tables | equalList (removeDuplicates (getTablesVars tables)) (convertAsVars asVars) = printAsVars (convertAsVars asVars) tables
                         | otherwise = error "All Variables should be declared as AS Vars"

-- Evalutes the AsVars for a FromGetWhere
-- ERROR: Returns an error for the case where not all Variables declared are listed in the 'As' Statement
evalAsVars' :: AsVars -> Equals -> Tables -> String
evalAsVars' asVars equals tables | equalList (removeDuplicates (getTablesVars tables)) (convertAsVars asVars) = printAsVars' (convertAsVars asVars) (convertEquals equals) tables
                                 | otherwise = error "All Variables should be declared as AS Vars"

{- ==========================================================  EvalAsVar AUX  ============================================================ -}

{- OVERVIEW 

-- printAsVars       => -returns output string for FromGetExpr
-- printAsVars'      => -returns output string for FromGetWhere

-- getNextAndCombine => -produces a list of all possible 'row' combinations called 'rows'

-- filterEqualVars   => -filters the list of 'rows' into a new list of 'rows' where equal vars have equal contents
-- filterExtQuant    => -filters the list of 'rows' into a new list of 'rows' where equal Ext. Quant. have equal contents
-- filterEqualities  => -filters the list of 'rows' into a new list of 'rows' where for each equality pair they have equal contents

-- processExtQuant   => -processes the list of 'rows' removing the columns which show Ext. Quant. values 
-- orderAs           => -processes the list of 'rows' formating them in the format described in the As statement 

-}

-- Evaluates the AsVars returning the Output String, WITHOUT any EQUALITY! 
-- Parameter $1: List of asVars denoting the ordering; e.g. ["x1","x3","x2"] [String]
printAsVars :: [String] -> Tables -> String
printAsVars asVars tables | areDuplicateVars tables = concat ( map (orderAs asVars) outputRowsDupVars)
                          | otherwise               = concat ( map (orderAs asVars) outputRows)
                          where outputRows          = processExtQuant (filterExtQuant (getNextAndCombine [[]] tables))
                                outputRowsDupVars   = filterEqualVars outputRows

-- Evaluates the AsVars returning the Output String, WITH EQUALITY! 
-- Parameter $1: List of asVars denoting the ordering; e.g. ["x1","x3","x2"] 
printAsVars' :: [String] -> [(String,String)] -> Tables -> String
printAsVars' asVars equals tables | areDuplicateVars tables = concat ( map (orderAs asVars) outputRowsDupEqVars)
                                  | otherwise               = concat ( map (orderAs asVars) outputRowsEqVars)
                                  where outputRows          = processExtQuant (filterExtQuant (getNextAndCombine [[]] tables))
                                        outputRowsEqVars    = (filterEqualities outputRows equals)
                                        outputRowsDupEqVars = (filterEqualVars outputRowsEqVars)

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
getNextAndCombine :: [[(Basic,String)]] -> Tables -> [[(Basic,String)]]
getNextAndCombine combinations ((_,table):[])     = [ c ++ getRow table x | c <- combinations, x <-[0..(getNumberOfRows table -1)]]
getNextAndCombine combinations ((_,table):tables) = getNextAndCombine ([c ++ getRow table x | c <- combinations, x <-[0..(getNumberOfRows table -1)]]) tables


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
filterExtQuant :: [[(Basic, String)]] -> [[(Basic, String)]]
filterExtQuant rows = [ row | row <- rows, allExtQuantEqual row]

allExtQuantEqual :: [(Basic, String)] -> Bool
allExtQuantEqual (_:[])                    = True
allExtQuantEqual (((VarBasic _), content):row)   = allExtQuantEqual row
allExtQuantEqual (((SomeBasic v), content):row)  = (allExtQuantEqual' v content row) && allExtQuantEqual row

allExtQuantEqual' :: String -> String -> [(Basic, String)] -> Bool
allExtQuantEqual' someVar content (((SomeBasic var), content2):[])  | someVar == var && content == content2 = True
                                                                    | someVar /= var                        = True
                                                                    | otherwise                             = False

allExtQuantEqual' _ _ (((VarBasic _), content2):[])                                                         = True

allExtQuantEqual' someVar content (((SomeBasic var), content2):row) | someVar == var && content == content2 = True && (allExtQuantEqual' someVar content row)
                                                                    | someVar /= var                        = True && (allExtQuantEqual' someVar content row)
                                                                    | otherwise                             = False

allExtQuantEqual' someVar content (((VarBasic _), content2):row)                                            = True && (allExtQuantEqual' someVar content row)
                                                               
-- Evaluates each row for all equalities, checking the contents of variables that are meant to be equal and returning those whose content do match
-- Parameter $1: List of every row, a single row can be described as such: [(x1,"content"), (x2, "content"), (x3,"content")]
-- Parameter $2: List of all equalities required ; e.g. [("x1","x2"),("x3","x4")] means x1=x2 and x3=x4 
filterEqualities :: [[(String,String)]] -> [(String,String)] -> [[(String,String)]]
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

-- Returns an output of all the rows but with individual columns of 'Some' variables removed and Basic-> String, since all will be vars 
-- e.g. The Row [(VarBasic v, 3), (SomeBasic k, 2), (SomeBasic k, 2)] ==> [(v, 3)]
processExtQuant :: [[(Basic,String)]] -> [[(String,String)]]
processExtQuant []       = []
processExtQuant (r:[])   = [processExtQuant' r]
processExtQuant (r:rows) = [processExtQuant' r] ++ processExtQuant rows

processExtQuant' :: [(Basic,String)] -> [(String,String)]
processExtQuant' ((VarBasic v, content):[])  = [(v,content)]
processExtQuant' ((SomeBasic _, content):[]) = []
processExtQuant' ((VarBasic v, content):vs)  = [(v,content)] ++ processExtQuant' vs
processExtQuant' ((SomeBasic _, content):vs) = processExtQuant' vs

-- Parameter $1: all Rows in form (Variable, Content) so e.g if r1 x1, (Row 1, Column x1) contains "Hello" then this will be [("x1", "Hello")]
-- Parameter $2: the AsVar variables e.g in the form such ["x1","x4","x2","x3"] denotes the printing out in this order                                 
orderAs :: [String] -> [(String, String)] -> String 
orderAs (asVar:[]) vcs = orderAs' vcs asVar ++ "\n"
orderAs (asVar:asVars) vcs = (orderAs' vcs asVar) ++ "," ++ orderAs asVars vcs

orderAs' :: [(String, String)] -> String -> String
orderAs' []  _ = error "smth went wrong"
orderAs' ((var,content):vcs) v  | v == var = content
                                | otherwise = orderAs' vcs v

-- ==========================================================  TABLE MANIPULATORS  ====================================================================================

{- OVERVIEW

-- getTablesVars      => -returns a list of all variables in all tables in tables
-- getRow             => -returns a specified row by index in a given table
-- getNumberOfRows    => -returns a Int value of how many rows are in a given table

-- areDuplicateVars   => -decides if a given list of tables contains multiple of the same variables, returning bool

-}

-- Returns a list of all variables in all tables given as an arguement
-- e.g. ( (Relation "A", (Column "x1" [])), (Relation "B", (Column "x2" [])) ) ===> ["x1","x2"]
getTablesVars :: Tables -> [String]
getTablesVars ((_, table):[]) = getTableVars table
getTablesVars ((_, table):tables) = (getTableVars table) ++ (getTablesVars tables)

getTableVars :: Table -> [String]
getTableVars (Column (SomeBasic _) _)         = []
getTableVars (Column (VarBasic var) _)        = [var]
getTableVars (Columns (SomeBasic _) _ table)  = (getTableVars table)
getTableVars (Columns (VarBasic var) _ table) = [var] ++ (getTableVars table)

-- Returns the Row of a given index in a table in form [(column, content)]
-- e.g. (Columns x1 ["c","cc","ccc","cccc"] (Column x2 ["s","sa","sas","sasa"])) 4 ===> [(VarBasic x1,"cccc"),(VarBasic x2, "sasa")]
getRow :: Table -> Int -> [(Basic, String)]
getRow (Column b cs) index        = [(b,cs!!index)]
getRow (Columns b cs table) index = [(b,cs!!index)] ++ (getRow table index)

-- Returns the number of rows in a given table
-- e.g. (Column _ ["s","sa","sas","sasa"])  ===> 4
getNumberOfRows :: Table -> Int
getNumberOfRows (Column b [])            = 0
getNumberOfRows (Column b (c:cs))        = 1 + getNumberOfRows (Column b cs)
getNumberOfRows (Columns b [] table)     = 0
getNumberOfRows (Columns b (c:cs) table) = 1 + getNumberOfRows (Columns b (cs) table)


-- Decides if a list of tables contains any duplicate variables 
-- e.g. ( (Relation "A", (Column "x1" [])), (Relation "B", (Column "x2" [])) ) ===> False
-- e.g. ( (Relation "A", (Column "x1" [])), (Relation "B", (Column "x1" [])) ) ===> True
areDuplicateVars :: Tables -> Bool
areDuplicateVars tables | duplicates (getTablesVars tables) = True
                        | otherwise = False

-- =============================================================  ENV MAKERS  =======================================================================================

{- OVERVIEW

-- getFilePaths  => -returns a list of all Relation Symbol filepaths given a Program AST 
-- getVars       => -returns a list of typles of all Relation Symbols along with a list all of the Variables/Ext. Quant within this relation given a Program AST 

-- makeTables    => -makes a list of Tables, given a list of the content from each Relation and the associated list made by getVars 
-- makeEnv       => -makes
-}

-- Returns FILEPATHS from a program, in the format ["A.csv", "B.csv"] 
getFilePaths :: Program -> [FilePath]
getFilePaths (Program statement _ ) = getFilePaths'' statement

getFilePaths' :: Statements -> [FilePath]
getFilePaths' (FromGetExpr fromGet)              = getFilePaths'' fromGet
getFilePaths' (FromGetWhere fromGet _)           = getFilePaths'' fromGet
getFilePaths' (AnyExpr _ statement1 statement2 ) = getFilePaths' statement1 ++ getFilePaths' statement2
getFilePaths' (Any _ statement)                  = getFilePaths' statement 

getFilePaths'' :: FromGet -> [FilePath]
getFilePaths'' (FromGetAnd relation _ fromGet)   = getFilePaths'' relation ++ getFilePaths'' fromGet
getFilePaths'' (FromGet relation _)              = getFilePaths'' relation

getFilePaths''' :: Relation -> [FilePath]
getFilePaths''' (Relation r) = [filepath r]


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
getVars'' (FromGetAnd relation vars fromGet) = [(relation, (getVars''' toGet))] ++ getVars'' fromGet
getVars'' (FromGet relation vars)            = [(relation, (getVars''' toGet))]

getVars''' :: Vars -> [String]
getVars''' (Param (Var v))       = [v]
getVars''' (Params (Var v) vars) = [v] ++ getVars''' vars

-- Generate the Tables. 
-- Parameter $1: List of each Relation data in order e.g ["hi,bye", "low,high"]
-- Parameter $2: Output of GetVars, the Relation, in order, with the String assignments
makeTables :: [String] -> [(Relation, [String])] -> Tables 
makeTables (content:[]) ((relation, basics):[])       = [(relation, (makeTable content basics))]
makeTables (content:contents) ((relation, basics):ys) = [(relation, (makeTable content basics))] ++ (makeTables contents ys)
makeTables _ _                                        = error "There should be an error here"

makeTable :: String -> [String] -> Table
makeTable content basics = makeTable' (splitContents content) basics

makeTable' :: [[String]] -> [String] -> Table
makeTable' [] (b:[])              = Column b []
makeTable' [] (b:basics)          = Columns b [] (makeTable' [] basics)
makeTable' (c:[]) (b:[])          = Column b c
makeTable' (c:content) (b:basics) = Columns b c (makeTable' content basics)
makeTable' _ _                    = error "There should be an error here"


makeEnv :: Tables -> Program -> Env


-- ================================================================  AUX  =============================================================================================

{- OVERVIEW


-}


-- Turns the Data Type:  Vars ====> [String] ; Retains order
convertAsVars :: AsVars -> [String]
convertAsVars (AsVar (Var s))         = [s]
convertAsVars (AsVars (Var s) asVars) = [s] ++ (convertAsVars asVars)

-- converts Equals into a [(String,String)] such that [("x1","x2"),("x3","x4")] x1=x2 and x3=x4
convertEquals :: Equals -> [(String,String)]
convertEquals (EqualVar var1 var2)     = [(getStringVar var1, getStringVar var2)] 
convertEquals (EqualVars var1 var2 eq) = [(getStringVar var1, getStringVar var2)] ++ (convertEquals eq) 

-- takes the sting part of the Var: Var "x1" = "x1"
getStringVar :: Var -> String
getStringVar (Var v) = v

equalList :: [String] -> [String] -> Bool
equalList x y = null (x \\ y) && null (y \\ x)

removeDuplicates :: [String] -> [String]
removeDuplicates [] = []
removeDuplicates (x:xs) | x `elem` xs = removeDuplicates xs
                        | otherwise   = x:(removeDuplicates xs) 


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


removeWhiteSpace :: [String] -> [String]
removeWhiteSpace list = [ filter (/=' ') l | l <- list]

-- ================================================================  IO  =============================================================================================

{- OVERVIEW

-}

readFiles :: [FilePath] -> IO [String]
readFiles ss = mapM readFile ss 

filepath :: String -> FilePath
filepath s = (s ++ ".csv")

-- ================================================================  MAIN  =========================================================================================== 
main :: IO ()
main = do
    s <- getContents

    -- Assigns : ast, to the abstract syntax tree generated by lexer and parser
    let ast = caca (alexScanTokens s)

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

