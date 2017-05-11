module Lambda (
  eval1,
  analyzeExp
              )where

import Control.Monad.State
import qualified Text.Parsec as Parsec
import qualified Data.Map as M

import Parser
import Token

type Enviroment = M.Map Char Int
type EnvState = State (Enviroment, Int) Exp

test1 = (App (Lambda ('x', 0) (Var ('x', 0))) (Var ('y', 0)))
test2 = (App (Lambda ('x', 0)
           (Lambda ('y', 0)
            (App (Var ('x', 0)) (Var ('y', 0)))))
           (App (Var ('a', 0)) (Var ('b', 0))))
test3 = App (App (Lambda ('x', 0)
               (App (Lambda ('x', 0)
                     (Var ('x', 0)))
                     (Var ('x', 0))))
           (App (Var ('a', 0)) (Var ('b', 0))))
         (App (Var ('c', 0)) (Var ('d', 0)))

isNF :: Exp -> Bool
isNF (Lambda _ nf) = isNF nf
isNF e = isNAbsNF e

isNAbsNF :: Exp -> Bool
isNAbsNF (Var _) = True
isNAbsNF (App (Var _) v2) | isNF v2 = True
isNAbsNF _ = False

eval1 :: Exp -> Exp
eval1 (App t1@(App _ _) t2) = App (eval1 t1) t2
eval1 (App nanf t) | isNAbsNF nanf = App nanf (eval1 t)
eval1 (Lambda x t) = Lambda x (eval1 t)
eval1 (App (Lambda x t) t2) = betaReduction x t t2
eval1 t = t

betaReduction :: Identifier -> Exp -> Exp -> Exp
betaReduction replaced term sbst = go term
  where
    go :: Exp -> Exp
    go (Var x) | x == replaced = sbst
    go (Var x) = Var x
    go (Lambda x t) = Lambda x (go t)
    go (App t1 t2) = App (go t1) (go t2)

analyzeExp :: Exp -> Exp
analyzeExp t = evalState (go t) (M.empty, 0) 
  where
    go :: Exp -> EnvState
    go t' = case t' of
      Var (x, _) -> do
        (memo, _) <- get
        let y = M.lookup x memo
        case y of
          Nothing -> return $ Var (x, -1)
          Just y' -> return $ Var (x, y')
      Lambda (x, _) t1 -> do
        (memo, n) <- get
        put (M.insert x n memo, n+1)
        t1' <- go t1
        return $ Lambda (x, n) t1'
      App t1 t2 -> do
        (memo, n) <- get
        t1' <- go t1
        put (memo, n)
        t2' <- go t2
        return $ App t1' t2'
