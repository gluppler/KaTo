{-# LANGUAGE OverloadedStrings #-}

module Normalizer (normalizeTokens, displayNormalizedTokens, normalize) where  -- Module definition and exported functions

import qualified Data.Text as T           -- Importing and aliasing 'Data.Text' for text operations

-- Normalizes tokens by converting to lowercase (Pure Function, Immutability, Higher-Order Functions)
normalizeTokens :: [String] -> [String]
normalizeTokens = map (T.unpack . T.toLower . T.pack)  -- Function composition with higher-order function `map`
-- - **Pure Function**: Given the same input, it always returns the same output without side effects.
-- - **Immutability**: It does not modify the original list; it produces a new one.
-- - **Higher-Order Function**: Uses `map` to apply a transformation to each element.

-- Display normalized tokens (Monads, List Processing, Higher-Order Functions)
displayNormalizedTokens :: [String] -> IO ()
displayNormalizedTokens normalized = do    -- IO Monad for handling side effects (printing to console)
    putStrLn $ "Normalized Tokens: " ++ show normalized           -- Print header for normalized tokens              -- Iterate over tokens and print each one (mapM_ for IO)
-- - **Monads**: The `IO` type is a Monad that allows for side effects, like printing to the console.
-- - **Higher-Order Functions**: Uses `mapM_` to perform an action (printing) on each element of the list.

-- Function to normalize a token by converting to lowercase and trimming whitespace (Pure Function, Immutability)
normalize :: String -> String
normalize token = T.unpack $ T.strip (T.toLower (T.pack token))    -- Function composition to normalize and trim tokens
-- - **Pure Function**: Takes a single token and transforms it without side effects.
-- - **Immutability**: Returns a new string without altering the input.
-- - **Function Composition**: Combines multiple functions into a single expression to process the input in a clear manner.

