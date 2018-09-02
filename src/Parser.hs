{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Language
import Data.Char
import Control.Monad
import Data.Attoparsec.Text
import qualified Data.Text as Text
import qualified Data.Map as Map

parse :: Text.Text -> Either String (Bindings Text.Text)
parse text = do
    bindings <- parseOnly (many' binding <* endOfInput) text
    bindings' <- 
        let f m (name, value) = do
                when (Map.member name m)
                    (error $ "Second definition of symbol \"" 
                        ++ Text.unpack name ++ "\"")
                pure $ Map.insert name value m
        in foldM f Map.empty bindings
    pure $ Bindings bindings'

binding :: Parser (Text.Text, Expression Text.Text)
binding = do
    name <- identifier 
    skipSpaces
    string "="
    skipSpaces
    value <- expression
    skipSpaces
    pure (name, value)

expression :: Parser (Expression Text.Text)
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

identifier :: Parser Text.Text
identifier = do
    name <- many1 letter
    pure $ Text.pack name

skipSpaces :: Parser ()
skipSpaces = skipWhile isSpace
