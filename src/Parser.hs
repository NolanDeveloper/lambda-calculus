{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Language
import Data.Text
import Data.Char
import Data.Attoparsec.Text as Atto
import Control.Monad
import qualified Data.Map as Map

parse :: Text -> Either String (Map.Map Text (Expression Text))
parse text = do
    bindings <- parseOnly (many' binding) text
    bindings' <- 
        let f m (name, value) = do
                when (Map.member name m)
                    (error $ "Second definition of symbol \"" 
                        ++ unpack name ++ "\"")
                pure $ Map.insert name value m
        in foldM f Map.empty bindings
    pure $ Map.fromList bindings

binding :: Parser (Text, Expression Text)
binding = do
    name <- identifier 
    skipSpaces
    string "="
    skipSpaces
    value <- expression
    skipSpaces
    pure (name, value)

expression :: Parser (Expression Text)
expression = choice 
    [ Identifier <$> identifier
    , do
        string "(\\"
        skipSpaces
        parameter <- identifier
        skipSpaces
        string "->"
        skipSpaces
        body <- expression
        string ")"
        pure $ Lambda parameter body
    , do
        string "("
        left <- expression 
        skipSpaces
        right <- expression
        string ")"
        pure $ Application left right
    ] 

identifier :: Parser Text
identifier = do
    name <- many1 letter
    pure $ pack name

---------------
-- Helpers

skipSpaces :: Parser ()
skipSpaces = skipWhile isSpace
