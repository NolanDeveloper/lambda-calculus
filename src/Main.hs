{-# LANGUAGE OverloadedStrings #-}
module Main where

import Language

import Paths_lambda_calculus
import Data.Text as T
import qualified Data.Text.IO as T
import Parser
import Control.Monad

readDataFile :: String -> IO Text
readDataFile fileName = getDataFileName fileName >>= T.readFile 

main :: IO ()
main = do
    content <- readDataFile "data/source.txt"
    case parse content of
        Left error -> do
            putStr "Error: " 
            putStrLn error
        Right definitions ->
            forM_ definitions $ \(name, value) -> do
                T.putStr name
                T.putStr " = "
                putStrLn $ show value
    print content
