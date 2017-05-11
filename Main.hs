import Text.Parsec

import Parser
import Lambda



main :: IO ()
main = do
  str <- getLine
  case str of
    ":q" -> return ()
    _ -> let e = toToken str in
         case e of
           Left msg -> putStrLn (show msg ++ "\n") >> main
           Right e' ->  do
             let e'' = analyzeExp e'
             putStrLn $ "input:\t" ++ toStr e''
             putStrLn $ "eval:\t" ++ toStr (eval1 e'')
             putStrLn ""
             main
