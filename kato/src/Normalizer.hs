{-# LANGUAGE OverloadedStrings #-}

module Normalizer (normalizeTokens, displayNormalizedTokens, normalize) where

import qualified Data.Text as T
import qualified Data.List as List  -- Add this import

-- Normalizes tokens by converting to lowercase
normalizeTokens :: [String] -> [String]
normalizeTokens = map (T.unpack . T.toLower . T.pack)

-- Display normalized tokens with counts
displayNormalizedTokens :: [String] -> IO ()
displayNormalizedTokens normalized = do
    putStrLn $ "Normalized Token Count: " ++ show (length normalized)
    let uniqueNormalized = List.nub normalized
    putStrLn $ "Unique Normalized Tokens: " ++ show (length uniqueNormalized)
    putStrLn "Normalized Tokens: "
    mapM_ putStrLn uniqueNormalized

-- Function to normalize a token by converting to lowercase and trimming whitespace
normalize :: String -> String
normalize token = T.unpack $ T.strip (T.toLower (T.pack token))

