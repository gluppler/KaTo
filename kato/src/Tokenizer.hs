{-# LANGUAGE OverloadedStrings #-}

module Tokenizer (tokenize, displayTokens) where   -- Module definition, exporting `tokenize` and `displayTokens`

import qualified Data.Text as T                   -- Importing and aliasing 'Data.Text' for text operations
import Data.Char (isAlphaNum, isSpace)            -- Importing character functions for filtering (alphanumeric and whitespace)

-- Tokenizes input text into words while handling punctuation (Pure Function, Immutability)
tokenize :: String -> [String]
tokenize input = filter (not . T.null . T.pack) $ map T.unpack $ T.words $ T.pack cleanedInput
  where
    cleanedInput = T.unpack . T.filter (\c -> isAlphaNum c || isSpace c) $ T.pack input   -- Using Lambda for filtering characters

-- Display tokens without counts (Monads, List Processing, Higher-Order Functions)
displayTokens :: [String] -> IO ()
displayTokens tokens = do     -- IO Monad to handle side effects (printing to console)
    putStrLn $ "Tokens: " ++ show tokens
                           

