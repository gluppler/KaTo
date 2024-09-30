{-# LANGUAGE OverloadedStrings #-}

module Normalizer (normalizeTokens, displayNormalizedTokens, normalize) where  -- Module definition and exported functions

import qualified Data.Text as T           -- Importing and aliasing 'Data.Text' for text operations
import qualified Data.List as List        -- Importing 'Data.List' for list operations (e.g., `nub`)

-- Normalizes tokens by converting to lowercase (Pure Function, Immutability, Higher-Order Functions)
normalizeTokens :: [String] -> [String]
normalizeTokens = map (T.unpack . T.toLower . T.pack)  -- Function composition with higher-order function `map`

-- Display normalized tokens with counts (Monads, List Processing, Higher-Order Functions)
displayNormalizedTokens :: [String] -> IO ()
displayNormalizedTokens normalized = do    -- IO Monad for handling side effects (printing to console)
    putStrLn $ "Normalized Token Count: " ++ show (length normalized)     -- Print total number of normalized tokens
    let uniqueNormalized = List.nub normalized                            -- List processing to remove duplicates
    putStrLn $ "Unique Normalized Tokens: " ++ show (length uniqueNormalized)  -- Print unique token count
    putStrLn "Normalized Tokens: "
    mapM_ putStrLn uniqueNormalized                                         -- Iterate over unique tokens and print each one (mapM_ for IO)

-- Function to normalize a token by converting to lowercase and trimming whitespace (Pure Function, Immutability)
normalize :: String -> String
normalize token = T.unpack $ T.strip (T.toLower (T.pack token))    -- Function composition to normalize and trim tokens

