module Token where

type Identifier = (Char, Int)

data Exp = Var Identifier
         | Lambda Identifier Exp
         | App Exp Exp deriving Show

instance Eq Exp where
  Var x == Var y = x == y
  Lambda x t1 == Lambda y t2 = x == y && t1 == t2
  App t1 t2 == App s1 s2 = t1 == s1 && t2 == s2
  _ == _  = False
  
toStr :: Exp -> String
toStr (Var (c, _)) = [c]
toStr (Lambda (c, _) e2) = ['(','\\',c] ++ " -> " ++ toStr e2 ++ ")"
toStr (App e1 e2) =  "(" ++ toStr e1 ++ ")" ++ "(" ++ toStr e2 ++  ")"
