module Parser (
  run,
  toStr,
  toToken
              )where


import Control.Applicative ((<*>), (*>), (<*), (<$>))
import Text.Parsec
import Text.Parsec.String

import Token

run :: String -> String
run input = run' input False

run' :: String -> Bool -> String
run' input b = case parse app "parse error" input of
              Left err -> show err
              Right val | not b -> toStr val
              Right val  -> show val

toToken :: String -> Either ParseError Exp
toToken input = parse app "parse error" input
              
app :: Parser Exp
app = do
  l <- lambda
  (App l <$> app) <|> pure l

lambda :: Parser Exp
lambda = do
      _ <- char '\\'
      spaces
      c <- oneOf ['a'..'z']
      spaces
      (Lambda (c, 0) <$> (string "->" *> app))
    <|> paren

paren :: Parser Exp
paren = spaces *> (char '(' *> app <* char ')'  <|> var ) <* spaces

var :: Parser Exp
var = (\c -> Var (c, 0)) <$> oneOf ['a'..'z'] <|> num

num :: Parser Exp
num = Lambda ('f', 0) . Lambda ('x', 0) . go . read <$> many1 digit
   where
     go :: Int -> Exp
     go n = foldr (\_ b -> App (Var ('f', 0)) b) (Var ('x', 0)) [1.. n]
     
