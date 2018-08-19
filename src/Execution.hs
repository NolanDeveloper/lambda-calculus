module Execution where

import Language
import Renamer
import qualified Data.Map as Map
import Data.Monoid

type Environment = Map.Map Variable (Expression Variable)

execute :: Environment -> Expression Variable -> IO ()
execute environment expression = go expression 0
  where 
    go expression n = do
        putStr $ show n
        putStr $ ") "
        putStrLn $ show expression
        case evaluateOneStepStrict environment expression of
            Nothing -> pure ()
            Just nextExpression -> go nextExpression (n + 1)

evaluateOneStep :: Environment -> Expression Variable -> Maybe (Expression Variable)
evaluateOneStep environment expression
    | Identifier id <- expression 
        = environment Map.!? id
    | Application function argument <- expression
    , Lambda parameter body <- function
        = Just $ substitute parameter argument body
    | Application function argument <- expression
        = let a = Application <$> evaluateOneStep environment function <*> pure argument
              b = Application function <$> evaluateOneStep environment argument
              f Nothing y = y
              f x _ = x
          in f a b
    | Lambda parameter body <- expression 
        = Lambda parameter <$> evaluateOneStep environment body
            
substitute :: Variable -> Expression Variable -> Expression Variable -> Expression Variable
substitute parameter argument body
    | Identifier id <- body
    , parameter == id
        = argument
    | Identifier id <- body
        = body
    | Application function' argument' <- body
        = let function'' = substitute parameter argument function'
              argument'' = substitute parameter argument argument'
          in Application function'' argument''
    | Lambda parameter' body' <- body
    , parameter' == parameter
        = body
    | Lambda parameter' body' <- body
        = Lambda parameter' (substitute parameter argument body')

evaluateOneStepStrict :: Environment -> Expression Variable -> Maybe (Expression Variable)
evaluateOneStepStrict environment expression
    | Identifier id <- expression 
        = environment Map.!? id
    | Application function argument <- expression
        = let a = First $ Application function <$> evaluateOneStep environment argument
              b = First $ Application <$> evaluateOneStep environment function <*> pure argument
              Lambda parameter body = function
              c = First $ Just $ substitute parameter argument body
          in getFirst $ a <> b <> c
    | Lambda parameter body <- expression 
        = Lambda parameter <$> evaluateOneStep environment body
