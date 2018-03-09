module Main where
import Parser
import Tokens
<<<<<<< HEAD
import Data.Csv


eval :: Program -> String
eval (Program s) = evalStatements s

evalStatements :: Statements -> String
evalStatements (FromGetExpr fromGet _) = evalFromGet fromGet 
eval1Statements (FromGetWhere fromGet _ _) = evalFromGet fromGet

evalFromGet :: FromGet -> String
evalFromGet (FromGetAnd rel _ _) = evalRelation rel
evalFromGet (FromGet rel toGet) = evalToGet rel toGet

evalToGet :: Relation -> ToGet -> String
evalToGet rel (Params1 s) = evalRelation rel ++ evalVar s


evalVar :: Var -> String
evalVar (Var a) = a

evalRelation :: Relation -> String
evalRelation (Relation s) = s ++ ".csv"

--getting relation
--getRelation :: Program -> String
--getRelation (Program (FromGetExpr fromGet _)) = evalFG fromGet 
--getRelation (Program (FromGetWhere fromGet _)) = evalFG fromGet 

--evalFG :: FromGet -> String
--evalFG (FromGetAnd (Relation re) _ _) = re ++ ".csv"
--(FromGet (Relation re) toGet) = re ++ ".csv"
=======
import Control.Monad
import Data.Char


-- A.csv CC, Sasa
-- Columns varName ["CC"] (Column varName ["Sasa"])

-- B.csv 1,2,3 
-- Columns (Info "1", Columns ( Info "2" , (Column (Info "3"))))
>>>>>>> 3abdaf4438f89c9a73543abe17921fcb525bb260


-- C.csv 1,2,3 \n 4,5,6
-- Columns (MoreInfo ("1", (Info "4")), Columns ( MoreInfo ("2", (Info "5")), (Column (MoreInfo ("3", (Info "6"))))))

-- Columns ()


-- 
data Table = Column String [String] | Columns String [String] Table deriving Show

-- EVAL

eval :: Program -> Table -> String
eval (Program (FromGetExpr fromGet vars)) table = evalFromGetExpr fromGet vars table
eval (Program (FromGetWhere fromGet equals vars)) _ = ""

evalFromGetExpr :: FromGet -> Vars -> Table -> String
evalFromGetExpr _ asVars table = evalAsVars asVars table

evalAsVars :: Vars -> Table -> String 
evalAsVars vars table = smth (evalAsVars' vars) table

evalAsVars' :: Vars -> [String]
evalAsVars' (Vars1 (Var s)) = [s]
evalAsVars' (Vars2 (Var s) vars) = [s] ++ (evalAsVars' vars)

smth :: [String] -> Table -> String
smth (v:[]) (Columns name (content:contents) table)   | v==name = content
                                                      | otherwise = smth [v] table
smth (v:[]) (Column name (content:contents))          | v==name = content
                                                      | otherwise = ""
smth (v:vars) (Columns name (content:contents) table) | v==name = content ++ "," ++ smth vars table
                                                      | otherwise = (smth [v] table) ++ "," ++ (smth vars (Columns name (content:contents) table))
smth (v:vars) (Column name (content:contents))        | v==name = content
                                                      | otherwise = ""




-- SMTH

-- Gets A SINGLE Relational symbol
getRelation :: Program -> String
getRelation (Program (FromGetExpr fromGet _)) = getRelation' fromGet
getRelation (Program (FromGetWhere fromGet _ _)) = getRelation' fromGet

getRelation' :: FromGet -> String 
getRelation' (FromGet (Relation symbol) _) = symbol
getRelation' (FromGetAnd (Relation symbol) _ _) = symbol

-- Gets the Variables designated
getVars :: Program -> [String]
getVars (Program (FromGetExpr fromGet _)) = getVars' fromGet

getVars' :: FromGet -> [String]
getVars' (FromGet _ vars) = getVars'' vars
getVars' (FromGetAnd _ vars _ ) = getVars'' vars

getVars'' :: ToGet -> [String]
getVars'' (Params (Some s)) = [s]
getVars'' (Params1 (Var s)) = [s]
getVars'' (Params2 toget1 toget2) = getVars'' toget1 ++ getVars'' toget2


makeTable :: String -> [String]-> Int -> Table
makeTable string vars number = makeTable' (clean (wordsWhen (==',') string)) vars number

makeTable' :: [String]-> [String] -> Int -> Table
makeTable' (x:xs) (y:ys) number | number > 1  = Columns y [x] (makeTable' xs ys (number-1))
makeTable' (x:xs) (y:ys) number | number == 1 = Column y [x]



-- AUX 
clean :: [String] -> [String]
clean (x:[]) = wordsWhen (=='\n') x
clean (x:xs) = wordsWhen (=='\n') x ++ clean xs

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
<<<<<<< HEAD
    print (eval ast)
--      result <- parseFromFile csvFile "/Users/Sarah/Desktop/PLC CW/A.csv"
--      print result
=======

    relationContents <- readFile (getRelation ast ++ ".csv")
    print (eval ast (makeTable relationContents (getVars ast) (length (getVars ast))))

>>>>>>> 3abdaf4438f89c9a73543abe17921fcb525bb260
