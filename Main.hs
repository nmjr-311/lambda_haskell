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
           Left msg -> print msg >> main
           Right e' ->  do
             putStrLn $ "input:\t" ++ show e'
             let e'' = (eval1 . analyzeExp) e'
             putStrLn $ "eval:\t" ++ toStr e''
             main
