module Token where

type Identifier = (Char, Int)

data Exp = Var Identifier
         | Lambda Identifier Exp
         | App Exp Exp deriving Show

toStr :: Exp -> String
toStr (Var (c, _)) = show c
toStr (Lambda (c, _) e2) = ['\\',c] ++ " -> " ++ toStr e2
toStr (App e1 e2) =  "(" ++ toStr e1 ++ ")" ++ "(" ++ toStr e2 ++  ")"
