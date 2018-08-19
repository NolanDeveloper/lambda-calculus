module Language where

import Data.Text
import Data.Map

data Expression id
    = Identifier id
    | Application (Expression id) (Expression id)
    | Lambda id (Expression id)

instance Show id => Show (Expression id) where
    show (Identifier name) = show name
    show (Application f x) = "(" ++ show f ++ " " ++ show x ++ ")"
    show (Lambda x e) = "(\\" ++ show x ++ " -> " ++ show e ++ ")"
