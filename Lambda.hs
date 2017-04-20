module Lambda where

import Control.Monad.State
import qualified Text.Parsec as Parsec

import Parser
import Token

type ReplaceState = State (Char, Exp, Exp) Exp
type AppError = String

toStrExp :: Either AppError Exp -> String
toStrExp (Left msg) = msg
toStrExp (Right e) = toStr e

eval :: Either Parsec.ParseError Exp -> Either AppError Exp
eval (Right (App (Lambda arg body) exp')) = eval (Right (replaceChar arg exp' body))
eval other = eval1 other

eval1 :: Either Parsec.ParseError Exp -> Either AppError Exp
eval1 (Left _) = Left "parse error"
eval1 (Right (Var s)) = Right (Var s)
eval1 (Right (Lambda e1 e2)) = case eval1 (Right e2) of
                                 Left msg -> Left msg
                                 Right e2' -> Right $ Lambda e1 e2'
--eval1 (Right (App (Lambda arg body) exp')) = Right (replaceChar arg exp' body)

eval1 (Right (App e1 e2)) = case e1 of
                              Lambda arg body -> Right (replaceChar arg e2 body)
                              _ -> case eval1 (Right e2) of
                                     Left msg -> Left msg
                                     Right e2' -> Right (App e1 e2')


replaceChar :: (Char, Int) -> Exp -> Exp -> Exp
replaceChar arg exp' body = case body of
                             Var c | c == arg -> exp'
                             Lambda e1 e2 | arg == e1 -> replaceChar arg exp' (alphaTranslate e1 e2)
                             Lambda e1 e2 -> Lambda e1 (replaceChar arg exp' e2)
                             App e1 e2 -> App (replaceChar arg exp' e1) (replaceChar arg exp' e2)
                             _ -> body

alphaTranslate :: Identifier -> Exp -> Exp
alphaTranslate arg body =
  case body of
    Var (c, i) | (c, i) == arg -> Lambda (c, i+1) $ Var (c, i+1)
    Lambda _ body' -> Lambda arg (alphaTranslate arg body')
    App e1 e2 -> App (alphaTranslate arg e1) (alphaTranslate arg e2)
    other -> other
