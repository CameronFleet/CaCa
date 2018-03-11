
import Parser
import Tokens
import Control.Monad
import Data.Char
import Data.List
import Data.List

-- Type of all of the Relations and their corrosponding Tables; what for?
-- Realtion == Relation "A" aka table for A.csv
type Tables = [(Relation, Table)] 

-- Representation of all the information for a single Relational Symbol
data Table = Column String [String] | Columns String [String] Table deriving Show


-- ================================================================  EVAL  ============================================================================================
eval :: Program -> Tables -> String
eval (Program (FromGetExpr fromGet vars)) tables         = evalFromGetExpr fromGet vars tables
eval (Program (FromGetWhere fromGet equals vars)) tables = evalFromGetWhere fromGet equals vars tables

evalFromGetExpr :: FromGet -> AsVars -> Tables -> String
evalFromGetExpr _ asVars tables = evalAsVars asVars tables


evalFromGetWhere :: FromGet -> Equals -> AsVars -> Tables -> String
evalFromGetWhere _ equals asVars tables = evalAsVars' asVars equals tables

-- Checks if the list of all Variables in Table, e.g ["x1","x2","x3"] (Those defined in fromgets)
evalAsVars :: AsVars -> Tables -> String
evalAsVars asVars tables | equalList (removeDuplicates (getTablesVars tables)) (convertAsVars asVars) = printAsVars (convertAsVars asVars) tables
                         | otherwise = error "All Variables should be declared as AS Vars"

-- TODO: make a 'convertEquals' function that converts Equals into a [(String,String)] such that [("x1","x2"),("x3","x4")] x1=x2 and x3=x4; Can be in AUX
evalAsVars' :: AsVars -> Equals -> Tables -> String
evalAsVars' asVars equals tables | equalList (removeDuplicates (getTablesVars tables)) (convertAsVars asVars) = printAsVars' (convertAsVars asVars) (convertEquals equals) tables
                                 | otherwise = error "All Variables should be declared as AS Vars"

-- ============================================================  EvalAsVar AUX  ========================================================================================

-- =========== EVAL FROM GET WHERE
-- TODO: Do exactly the same as printAsVars but only keep those rows where the variables in [(String,String)] are equal! 
printAsVars' :: [String] -> [(String,String)] -> Tables -> String
printAsVars' asVars equals tables | areDuplicateVars tables = concat ( map (orderAs asVars) outputRowsDupEqVars)
                                  | otherwise               = concat ( map (orderAs asVars) outputRowsEqVars)
                                  where outputRowsEqVars = (keepRowsWithEqVars (getNextAndCombine [[]] tables) equals)
                                        outputRowsDupEqVars = (keepRowsWithDup outputRowsEqVars)

keepRowsWithEqVars :: [[(String,String)]] -> [(String,String)] -> [[(String,String)]]
keepRowsWithEqVars rows (eq:[])  = rowsEqVars rows eq
keepRowsWithEqVars rows (eq:eqs) = keepRowsWithEqVars (rowsEqVars rows eq) eqs 

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

-- =========== EVAL FROM GET EXPR 
printAsVars :: [String] -> Tables -> String
printAsVars asVars tables | areDuplicateVars tables = concat ( map (orderAs asVars) (keepRowsWithDup (getNextAndCombine [[]] tables)))
                          | otherwise = concat ( map (orderAs asVars) (getNextAndCombine [[]] tables))


-- getting the next table and making the combinations with the curr
getNextAndCombine :: [[(String,String)]] -> Tables -> [[(String,String)]]
getNextAndCombine combinations ((_,table):[]) = [ c ++ getRow table x | c <- combinations, x <-[0..(getNumberOfRows table -1)]]
getNextAndCombine combinations ((_,table):tables) = getNextAndCombine ([c ++ getRow table x | c <- combinations, x <-[0..(getNumberOfRows table -1)]]) tables

-- keeping the rows that have duplicates
keepRowsWithDup :: [[(String,String)]] -> [[(String,String)]]
keepRowsWithDup combinations = [ c | c <- combinations, equalVars c]


-- [(x1,1),(x1,2),(x1,2),(x3,3)]
equalVars :: [(String,String)] -> Bool 
equalVars ((_,_):[])          = True
equalVars ((var,content):row) = (equalVars' var content row) && (equalVars row)

equalVars' :: String -> String -> [(String,String)] -> Bool
equalVars' var content ((var2,content2):[])  | var == var2 && content == content2 = True 
                                             | var /= var2                        = True
                                             | otherwise                          = False

equalVars' var content ((var2,content2):row) | var == var2 && content == content2 = True && (equalVars' var content row)
                                             | var /= var2                        = True && (equalVars' var content row)
                                             | otherwise                          = False

-- if there is a duplicate var:
areDuplicateVars :: Tables -> Bool
areDuplicateVars tables | duplicates (getTablesVars tables) = True
                        | otherwise = False
                  
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

getTablesVars :: Tables -> [String]
getTablesVars ((_, table):[]) = getTableVars table
getTablesVars ((_, table):tables) = (getTableVars table) ++ (getTablesVars tables)

getTableVars :: Table -> [String]
getTableVars (Column var _) = [var]
getTableVars (Columns var _ table) = [var] ++ (getTableVars table)

-- returns the Row of a given index in a table in form [(column, content)]
getRow :: Table -> Int -> [(String, String)]
getRow (Column var cs) index        = [(var,cs!!index)]
getRow (Columns var cs table) index = [(var,cs!!index)] ++ (getRow table index)

-- returns the number of rows in a given table
getNumberOfRows :: Table -> Int
getNumberOfRows (Columns var [] table) = 0
getNumberOfRows (Columns var (c:cs) table) = 1 + getNumberOfRows (Columns var (cs) table)

-- =============================================================  TABLE MAKERS  =======================================================================================

-- Get FILEPATHS , returns FILEPATHS 
getFilePaths :: Program -> [FilePath]
getFilePaths (Program (FromGetExpr fromGet _)) = getFilePaths' fromGet
getFilePaths (Program (FromGetWhere fromGet _ _)) = getFilePaths' fromGet

getFilePaths' :: FromGet -> [FilePath]
getFilePaths' (FromGetAnd relation _ fromGet) = getFilePaths'' relation ++ getFilePaths' fromGet
getFilePaths' (FromGet relation _) = getFilePaths'' relation

getFilePaths'' :: Relation -> [FilePath]
getFilePaths'' (Relation r) = [filepath r]


-- Get Relations Variables!, Each variable is assigned to a column in the table! 
getVars :: Program -> [(Relation,[String])]
getVars (Program (FromGetExpr fromGet _)) = getVars' fromGet
getVars (Program (FromGetWhere fromGet _ _)) = getVars' fromGet

getVars' :: FromGet -> [(Relation,[String])] 
getVars' (FromGetAnd relation toGet fromGet) = [(relation, (listVarsToGet toGet))] ++ getVars' fromGet
getVars' (FromGet relation toGet) = [(relation, (listVarsToGet toGet))]

listVarsToGet :: ToGet -> [String]
listVarsToGet (Params1(Var v)) = [v]
listVarsToGet (Params2 toGet toGet1) = listVarsToGet toGet ++ listVarsToGet toGet1

-- Generate the Tables. 
-- Parameter $1: List of each Relation data in order e.g ["hi,bye", "low,high"]
-- Parameter $2: Output of GetVars, the Relation, in order, with the String assignments
makeTables :: [String] -> [(Relation,[String])] -> Tables 
makeTables (content:[]) ((relation, vars):[]) = [(relation, (makeTable content vars))]
makeTables (content:contents) ((relation, vars):ys) = [(relation, (makeTable content vars))] ++ (makeTables contents ys)
makeTables _ _ = error "There should be an error here"

makeTable :: String -> [String] -> Table
makeTable content vars = makeTable' (splitContents content) vars

makeTable' :: [[String]] -> [String] -> Table
makeTable' [] (v:[]) = Column v []
makeTable' [] (v:vars) = Columns v [] (makeTable' [] vars)
makeTable' (c:[]) (v:[]) = Column v c
makeTable' (c:content) (v:vars) = Columns v c (makeTable' content vars)
makeTable' _ _ = error "There should be an error here"

-- ================================================================  AUX  =============================================================================================

-- Turns the Data Type:  Vars ====> [String] ; Retains order
convertAsVars :: AsVars -> [String]
convertAsVars (AsVar (Var s)) = [s]
convertAsVars (AsVars (Var s) asVars) = [s] ++ (convertAsVars asVars)

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
splitContents'' ss = [map head ss] ++ splitContents'' (map tail ss)

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


-- ================================================================  IO  =============================================================================================

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

    -- Assigns : tables, to be the table containing all info needed to parse correctly. e.g if the files above are name with variable names
    --  A(x1,x2) and B(x3,x4) then Tables will be the following
    -- [(Relation "A", Columns x1 ["hi"] (Column x2 ["bye"])), (Relation "B", Columns x3 ["low"] (Column x4 ["high"]))]
    -- makeTables.          ; Should generate of type Tables. 
    -- getVars.             ; Must include to which Relation each variable is related to, so maybe [(Relation "A", ["x1","x2"]), (Relation "B", ["x3","x4"])]
    let tables = makeTables relationContents (getVars ast)


    -- Prints : the full evaluation, eval ast tables for any given program defined by our BNF should produce the desired result with proper error handling.
    -- FromGetWhere and FromGet. ; Basics, get this working.
    -- TODO: AsVars and Equals.        ; Basics, get this working. AsVars is probably where the final printing occurs. (Maybe even all printing?)
    -- TODO: ToGet and Vars.           ; Simple. 
    -- TODO: Some                      ; This is probably going to be very Hard! And will probably make it very hard
    putStr (eval ast tables)

