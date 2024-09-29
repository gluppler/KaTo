{-# LANGUAGE OverloadedStrings #-}

module Tokenizer (tokenize, displayTokens) where

import qualified Data.Text as T
import qualified Data.List as List
import Data.Char (isAlphaNum, isSpace)

-- Tokenizes input text into words while handling punctuation
tokenize :: String -> [String]
tokenize input = filter (not . T.null . T.pack) $ map T.unpack $ T.words $ T.pack cleanedInput
  where
    cleanedInput = T.unpack . T.filter (\c -> isAlphaNum c || isSpace c) $ T.pack input

-- Display tokens with counts
displayTokens :: [String] -> IO ()
displayTokens tokens = do
    putStrLn $ "Token Count: " ++ show (length tokens)
    let uniqueTokens = List.nub tokens
    putStrLn $ "Unique Tokens: " ++ show (length uniqueTokens)
    putStrLn "Tokens: "
    mapM_ putStrLn uniqueTokens

