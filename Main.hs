import Text.Parsec

import Parser
import Lambda
import Token


main :: IO ()
main = do
  str <- getLine
  case str of
    ":q" -> return ()
    ":eval" -> do
      str' <- getLine
      oneIteration str' evalToValue
      main
    ":each" -> do
      str' <- getLine
      let e = toToken str' in
        case e of
          Left msg -> putStrLn (show msg ++ "\n") >> main
          Right e' -> do
            let t = analyzeExp e'
            putStrLn $ "input:\t" ++ toStr t
            eachIteration t
    _ -> do
      oneIteration str eval1
      main
  
oneIteration :: String -> (Exp -> Exp) -> IO ()
oneIteration str evalFunction =
  let e = toToken str in
    case e of
      Left msg -> putStrLn (show msg ++ "\n") >> main
      Right e' ->  do
        let e'' = analyzeExp e'
        putStrLn $ "input:\t" ++ toStr e''
        putStrLn $ "eval:\t" ++ toStr (evalFunction e'')
        putStrLn ""

evalToValue :: Exp -> Exp
evalToValue t = go t
  where
    go :: Exp -> Exp
    go t' = let _t' = eval1 t' in
           if t' == _t'
           then _t'
           else go _t'

eachIteration :: Exp -> IO ()
eachIteration t = do
        let e = eval1 t
        putStrLn $ "eval:\t" ++ toStr e
        if eval1 e == e
          then putStrLn ""
          else eachIteration e             
