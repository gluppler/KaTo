{-# LANGUAGE OverloadedStrings #-}

module Main where

import Tokenizer (tokenize, displayTokens)
import Normalizer (normalizeTokens, displayNormalizedTokens)
import Lemmatizer (lemmatize, displayLemmatizedTokens, defaultDict)

main :: IO ()
main = do
    putStrLn "Welcome to KaTo: A User Friendly CLI Based NLP Tool!"
    putStrLn "Please enter the text you want to process:"
    inputText <- getLine

    -- Tokenization
    let tokens = tokenize inputText
    displayTokens tokens  -- Displaying tokens directly without counts

    -- Normalization
    let normalizedTokens = normalizeTokens tokens
    displayNormalizedTokens normalizedTokens  -- Displaying normalized tokens without unique counts

    -- Lemmatization
    let lemmatizedTokens = map (`lemmatize` defaultDict) normalizedTokens
    displayLemmatizedTokens lemmatizedTokens  -- Displaying lemmatized tokens without unique counts

    putStrLn "Process completed successfully."

