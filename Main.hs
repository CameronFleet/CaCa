module Main where
import Parser
import Tokens


eval :: Program -> Int
eval (Program (FromGetExpr fromGet)) = eval2 fromGet
eval (Program (FromGetWhere fromGet eqls)) = eval2 fromGet + eval3 eqls

eval2 :: FromGet -> Int
eval2 (FromGet relation toget) = 5
eval2 (FromGetAnd relation toget fromGet) = 10 + eval2 fromGet

eval3 :: Equals -> Int
eval3 (EqualVar (Var s) (Var t)) = 15
eval3 (EqualVars (Var s) (Var t) eqls) = 20 + eval3 eqls


main :: IO ()
main = do
    s <- getContents
    let ast = caca (alexScanTokens s)
    print (eval ast)