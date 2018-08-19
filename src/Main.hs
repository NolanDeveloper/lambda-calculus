{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad
import qualified Data.Map as Map
import Data.List

import Paths_lambda_calculus

import Language
import Parser
import Renamer
import Execution

readDataFile :: String -> IO T.Text
readDataFile fileName = getDataFileName fileName >>= T.readFile 

main :: IO ()
main = do
    content <- readDataFile "data/source.txt"
    let bindingsAndMain = do
            bindings <- rename <$> parse content 
            case find (\(Variable _ name) -> name == "main") (Map.keys bindings) of
                Just main -> Right (bindings, main)
                Nothing -> Left "Can't find \"main\""
    case bindingsAndMain of
        Left error -> do
            putStr "Error: " 
            putStrLn error
        Right (bindings, main) -> do
            execute bindings (Identifier main)
