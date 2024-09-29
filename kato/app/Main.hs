{-# LANGUAGE OverloadedStrings #-}

module Main where

import Tokenizer (tokenize)
import Normalizer (normalize)
import Lemmatizer (lemmatize, displayLemmatizedTokens, defaultDict)
import qualified Data.Text as T
import Control.Monad (forM)

main :: IO ()
main = do
    putStrLn "Welcome to KaTo: A User Friendly CLI Based NLP Tool!"
    putStrLn "Please enter the text you want to process:"
    inputText <- getLine

    -- Tokenization
    let tokens = tokenize inputText
    putStrLn $ "Tokens: " ++ show tokens

    -- Normalization
    let normalizedTokens = map normalize tokens
    putStrLn $ "Normalized Tokens: " ++ show normalizedTokens

    -- Lemmatization
    let lemmatizedTokens = map (`lemmatize` defaultDict) normalizedTokens
    putStrLn "Lemmatized Tokens: "
    displayLemmatizedTokens lemmatizedTokens

    putStrLn "Process completed successfully."

