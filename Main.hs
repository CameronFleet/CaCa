module Main where
import Parser
import Tokens
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



main :: IO ()
main = do
    s <- getContents
    let ast = caca (alexScanTokens s)
    print (eval ast)
--      result <- parseFromFile csvFile "/Users/Sarah/Desktop/PLC CW/A.csv"
--      print result