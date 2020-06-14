{-|
Module      : C10P6
Description : Programming in Haskell, chapter 10, problem 6
Maintainer  : David Bruck
-}
module C10P6
    ( tautChecker
    ) where
import C10P6BookFunctions
    ( isTaut
    , Prop(..)
    )
import Parsing
    ( Parser(..)
    , parse
    , symbol
    , token
    , upper
    , (+++)
    )
import Prelude hiding
    ( Const
    )
import System.IO
    ( hFlush
    , stdout
    )

{-|
    Commandline interface accepting user input expressions as propositions for
    testing for tautology (evaluating if a boolean expression is always true
    for all combinations of parameters)
-}
tautChecker =
    do putStrLn "Tautology checker Commandline interface by David Bruck"
       putStrLn "Parses proposition then evaluates tautology via book method\n"
       putStrLn "Enter proposition using operators:"
       putStrLn "    \">\" : (Implies)"
       putStrLn "    \"!\" : (Not)"
       putStrLn "    \"&\" : (And)"
       putStrLn "    \"True\"/\"False\": (constant value)"
       putStrLn "    Characters A through Z : named boolean variables"
       putStrLn "    Parentheses around sub-expressions optional"
       putStrLn "Example: \"A & A\" prints \"False\""
       putStrLn "Example: \"(A > A) & (B > B)\" prints \"True\""
       putStrLn "Example: \"(A & !(!A)) > A\" prints \"True\"\n"
       putStr "Proposition: "
       hFlush stdout -- Write text to console immediately for Windows

       xs <- getLine
       case parse prop xs of
           [(n, [])]    -> putStrLn $ "Result: " ++ show (isTaut n)
           [(_, out)]   -> error ("unused input " ++ out)
           []           -> error "invalid input"

prop :: Parser Prop
prop = do l <- both
          do  symbol ">"
              r <- prop
              return $ l `Imply` r
           +++ return l

both :: Parser Prop
both = do l <- neg
          do symbol "&"
             r <- both
             return $ l `And` r
           +++ return l

neg :: Parser Prop
neg = do symbol "!"
         Not <$> factor
      +++ factor

factor :: Parser Prop
factor = do symbol "("
            p <- prop
            symbol ")"
            return p
          +++ tf

tf :: Parser Prop
tf = do symbol "True"
        return $ Const True
      +++ do symbol "False"
             return $ Const False
      +++ var

var :: Parser Prop
var = do v <- token upper
         return $ Var v
