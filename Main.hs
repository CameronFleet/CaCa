module Main where
import Parser
import Tokens
import Control.Monad
import Data.Char


-- A.csv CC, Sasa
-- Columns varName ["CC"] (Column varName ["Sasa"])

-- B.csv 1,2,3 
-- Columns (Info "1", Columns ( Info "2" , (Column (Info "3"))))


-- C.csv 1,2,3 \n 4,5,6
-- Columns (MoreInfo ("1", (Info "4")), Columns ( MoreInfo ("2", (Info "5")), (Column (MoreInfo ("3", (Info "6"))))))


-- Type of all of the Relations and their corrosponding Tables 
type Tables = [(Relation, Table)]

-- Representation of all the information for a single Relational Symbol
data Table = Column String [String] | Columns String [String] Table deriving Show

-- EVAL, The Whole evaluation of the AST

-- Evals a Program
eval :: Program -> Table -> String
eval (Program (FromGetExpr fromGet vars)) table = evalFromGetExpr fromGet vars table
eval (Program (FromGetWhere fromGet equals vars)) _ = ""

-- Evals a From _ Get; Expression 
evalFromGetExpr :: FromGet -> Vars -> Table -> String
evalFromGetExpr _ asVars table = evalAsVars asVars table

-- Evals a As _; Expression the 'as' vars, so eg. as name1, name 2 would return the string of : show(name1) ++ "," ++ show(name2)
evalAsVars :: Vars -> Table -> String 
evalAsVars vars table = evalAsVars' (varsToString vars) table


evalAsVars' :: [String] -> Table -> String
evalAsVars' (v:[]) (Columns name (content:contents) table)   | v==name = content
                                                             | otherwise = evalAsVars' [v] table

evalAsVars' (v:[]) (Column name (content:contents))          | v==name = content
                                                             | otherwise = ""

evalAsVars' (v:vars) (Columns name (content:contents) table) | v==name = content ++ "," ++ evalAsVars' vars table
                                                             | otherwise = (evalAsVars' [v] table) ++ "," ++ (evalAsVars' vars (Columns name (content:contents) table))

evalAsVars' (v:vars) (Column name (content:contents))        | v==name = content
                                                             | otherwise = ""


-- INFORMATION ABOUT THE AST, e.g the relational symbols, the amount of vars.. 

-- Gets A SINGLE Relational symbol
-- getRelation :: Program -> String
-- getRelation (Program (FromGetExpr fromGet _)) = getRelation' fromGet
-- getRelation (Program (FromGetWhere fromGet _ _)) = getRelation' fromGet

-- getRelation' :: FromGet -> String 
-- getRelation' (FromGet (Relation symbol) _) = symbol
-- getRelation' (FromGetAnd (Relation symbol) _ _) = symbol

-- Get FILEPATHS , returns FILEPATHS 
getFilePaths :: Program -> [FilePath]

-- Returns Designated Variables
getVars :: Program -> [String]
getVars (Program (FromGetExpr fromGet _)) = getVars' fromGet

getVars' :: FromGet -> [String]
getVars' (FromGet _ vars) = getVars'' vars
getVars' (FromGetAnd _ vars _ ) = getVars'' vars

getVars'' :: ToGet -> [String]
getVars'' (Params (Some s)) = [s]
getVars'' (Params1 (Var s)) = [s]
getVars'' (Params2 toget1 toget2) = getVars'' toget1 ++ getVars'' toget2

-- Generation of the Table Structure
makeTable :: String -> [String]-> Int -> Table
makeTable string vars number = makeTable' (clean (wordsWhen (==',') string)) vars number

makeTable' :: [String]-> [String] -> Int -> Table
makeTable' (x:xs) (y:ys) number | number > 1  = Columns y [x] (makeTable' xs ys (number-1))
makeTable' (x:xs) (y:ys) number | number == 1 = Column y [x]



-- AUX 

-- Turns the Data Type:  Vars ====> [String] ; Retains order
varsToString :: Vars -> [String]
varsToString (Vars1 (Var s)) = [s]
varsToString (Vars2 (Var s) vars) = [s] ++ (varsToString vars)


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


-- IO Functions 

readFiles :: [FilePath] -> IO [String]
readFiles ss = mapM readFile ss 

filepath :: String -> FilePath
filepath s = (s ++ ".csv")


-- readFiles (s:[]) things = do relationContents <- readFile (s ++ ".csv")
--                              return things
-- readFiles (s:ss) things = do
--                              relationContents <- readFile (s ++ ".csv")
--                              readFiles ss ([relationContents])
                      

-- MAIN 

main :: IO ()
main = do
    s <- getContents

    -- Assigns : ast, to the abstract syntax tree
    let ast = caca (alexScanTokens s)

    -- Assigns : relationContents, to a list containing all of the files info, e.g. file A contains : "hi,bye" and file B contanis "low,high"
    -- then relationContents will be = ["hi,bye", "low,high"]
    -- TODO: make getFilePaths         ; Will navigate the ast to fine any and all declarations of: from A, from B and produce [FilePath]: ["A.csv", "B.csv"]
    relationContents <- readFiles (getFilePaths ast)

    -- Assigns : tables, to be the table containing all info needed to parse correctly. e.g if the files above are name with variable names
    --  A(x1,x2) and B(x3,x4) then Tables will be the following
    -- [(Relation "A", Columns x1 ["hi"] (Column x2 ["bye"])), (Relation "B", Columns x3 ["low"] (Column x4 ["high"]))]
    -- TODO: make makeTables.          ; Should generate of type Tables. 
    -- TODO: re-make getVars.          ; Must include to which Relation each variable is related to, so maybe [(Relation "A", ["x1","x2"]), (Relation "B", ["x3","x4"])]
    let tables = makeTables relationContents (getVars ast)


    -- Prints : the full evaluation, eval ast tables for any given program defined by our BNF should produce the desired result with proper error handling.
    -- TODO: FromGetWhere and FromGet. ; Basics, get this working.
    -- TODO: FromGet and FromGetAnd.   ; Basics, get this working.
    -- TODO: AsVars and Equals.        ; Basics, get this working. AsVars is probably where the final printing occurs. (Maybe even all printing?)
    -- TODO: ToGet and Vars.           ; Simple. 
    -- TODO: Some                      ; This is probably going to be very Hard! And will probably make it very hard
    print (eval ast tables)


