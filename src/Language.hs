module Language(Expression(..), Literal(..)) where

import Data.Text

data Expression
    = Identifier Text
    | Literal Literal

data Literal
    = LText Text
    | LInteger Int

instance Show Expression where
    show (Identifier name) = unpack name
    show (Literal literal) = show literal
    
instance Show Literal where
    show (LText value) = "\"" ++ unpack value ++ "\""
    show (LInteger value) = show value
