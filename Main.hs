
import Parser
import Tokens
import Control.Monad
import Data.Char

-- Type of all of the Relations and their corrosponding Tables 
type Tables = [(Relation, Table)]

-- Representation of all the information for a single Relational Symbol
data Table = Column String [String] | Columns String [String] Table deriving Show


-- ================================================================  EVAL  ============================================================================================
eval :: Program -> Tables -> String
eval (Program (FromGetExpr fromGet vars)) tables         = evalFromGetExpr fromGet vars tables
eval (Program (FromGetWhere fromGet equals vars)) tables = evalFromGetWhere fromGet equals vars tables

evalFromGetExpr :: FromGet -> AsVars -> Tables -> String

evalFromGetWhere :: FromGet -> Equals -> AsVars -> Tables -> String



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
makeTable' (c:[]) (v:[]) = Column v c
makeTable' (c:content) (v:vars) = Columns v c (makeTable' content vars)
makeTable' _ _ = error "There should be an error here"


-- ================================================================  AUX  =============================================================================================

-- Turns the Data Type:  Vars ====> [String] ; Retains order
varsToString :: AsVars -> [String]
varsToString (AsVar (Var s)) = [s]
varsToString (AsVars (Var s) asVars) = [s] ++ (varsToString asVars)

-- "hi,bye" to [["hi"], ["bye"]]; "hi,bye\n zdraveyte,chao" to [["hi","zdraveyte"],["bye", "chao"]]
splitContents :: String -> [[String]]
splitContents s = splitContents'' (splitContents' (wordsWhen (=='\n') s))

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
    print (tables)

    -- Prints : the full evaluation, eval ast tables for any given program defined by our BNF should produce the desired result with proper error handling.
    -- TODO: FromGetWhere and FromGet. ; Basics, get this working.
    -- TODO: FromGet and FromGetAnd.   ; Basics, get this working.
    -- TODO: AsVars and Equals.        ; Basics, get this working. AsVars is probably where the final printing occurs. (Maybe even all printing?)
    -- TODO: ToGet and Vars.           ; Simple. 
    -- TODO: Some                      ; This is probably going to be very Hard! And will probably make it very hard
--    print (eval ast tables)

