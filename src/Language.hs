module Language where

import Data.Text
import Data.Map

data Module id
    = Module {
        moduleBindings :: Map id (Expression id)
    }

data Expression id
    = Identifier id
    | Literal (Either Text Int)
    | Application (Expression id) (Expression id)
    | Lambda id (Expression id)

instance Show id => Show (Expression id) where
    show (Identifier name) = show name
    show (Literal (Left value)) = "\"" ++ unpack value ++ "\""
    show (Literal (Right value)) = show value
    show (Application f x) = "(" ++ show f ++ " " ++ show x ++ ")"
    show (Lambda x e) 
        = "(\\" ++ show x ++ " -> " ++ show e ++ ")"
