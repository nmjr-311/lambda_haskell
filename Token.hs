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
toStr (Var (c, n)) = if n < 1
                     then [c]
                     else c : show n
toStr (Lambda (c, n) e2) = (if n < 1
                           then ['\\',c]
                           else ['\\', c] ++ show n)
                                ++ " -> " ++ toStr e2
toStr (App e1@(Lambda _ _) e2) =
  case e2 of
    Var _ -> "(" ++ toStr e1 ++ ")" ++ toStr e2
    _ -> "(" ++ toStr e1 ++ ")" ++ "(" ++ toStr e2 ++  ")"
toStr (App e1 e2@(App _ _)) = toStr e1 ++ "(" ++ toStr e2 ++  ")"
toStr (App e1 e2) =
  toStr e1 ++ toStr e2
