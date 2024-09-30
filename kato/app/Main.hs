{-# LANGUAGE OverloadedStrings #-}

module Main where

import Tokenizer (tokenize)
import Normalizer (normalizeTokens, displayNormalizedTokens)
import Lemmatizer (lemmatize, displayLemmatizedTokens, defaultDict)
import qualified Data.List as List

main :: IO ()
main = do
    putStrLn "Welcome to KaTo: A User Friendly CLI Based NLP Tool!"
    putStrLn "Please enter the text you want to process:"
    inputText <- getLine

    -- Tokenization
    let tokens = tokenize inputText
    putStrLn $ "Tokens: " ++ show tokens

    -- Normalization
    let normalizedTokens = normalizeTokens tokens
    let uniqueNormalizedTokens = List.nub normalizedTokens  -- Ensure unique normalized tokens
    displayNormalizedTokens uniqueNormalizedTokens

    -- Lemmatization
    let lemmatizedTokens = map (`lemmatize` defaultDict) uniqueNormalizedTokens
    let uniqueLemmatizedTokens = List.nub lemmatizedTokens  -- Ensure unique lemmatized tokens
    displayLemmatizedTokens uniqueLemmatizedTokens  -- Displaying unique lemmatized tokens

    putStrLn "Process completed successfully."

