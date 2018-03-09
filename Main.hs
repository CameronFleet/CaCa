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
getRelation :: Program -> String
getRelation (Program (FromGetExpr fromGet _)) = getRelation' fromGet
getRelation (Program (FromGetWhere fromGet _ _)) = getRelation' fromGet

getRelation' :: FromGet -> String 
getRelation' (FromGet (Relation symbol) _) = symbol
getRelation' (FromGetAnd (Relation symbol) _ _) = symbol

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
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


-- MAIN 

main :: IO ()
main = do
    s <- getContents
    let ast = caca (alexScanTokens s)

    relationContents <- readFile (getRelation ast ++ ".csv")
    print (eval ast (makeTable relationContents (getVars ast) (length (getVars ast))))

