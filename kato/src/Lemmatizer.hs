{-# LANGUAGE OverloadedStrings #-}

module Lemmatizer (lemmatize, displayLemmatizedTokens, defaultDict) where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.List as List

-- Define a type for the lemmatization dictionary
type LemmaDict = Map.Map String String

-- Further expanded lemmatization dictionary
defaultDict :: LemmaDict
defaultDict = Map.fromList [
    -- Regular verbs
    ("running", "run"),
    ("ran", "run"),
    ("runs", "run"),
    ("eating", "eat"),
    ("ate", "eat"),
    ("eats", "eat"),
    ("playing", "play"),
    ("played", "play"),
    ("plays", "play"),
    ("jumped", "jump"),
    ("jumping", "jump"),
    ("jumps", "jump"),

    -- Irregular verbs
    ("wrote", "write"),
    ("written", "write"),
    ("sang", "sing"),
    ("sung", "sing"),
    ("went", "go"),
    ("gone", "go"),
    ("saw", "see"),
    ("seen", "see"),
    ("broke", "break"),
    ("broken", "break"),

    -- Adjectives
    ("better", "good"),
    ("best", "good"),
    ("worse", "bad"),
    ("worst", "bad"),
    ("happier", "happy"),
    ("happiest", "happy"),
    ("simpler", "simple"),
    ("simplest", "simple"),
    ("smarter", "smart"),
    ("smartest", "smart"),

    -- Nouns
    ("children", "child"),
    ("mice", "mouse"),
    ("geese", "goose"),
    ("feet", "foot"),
    ("women", "woman"),
    ("men", "man"),
    ("teeth", "tooth"),
    ("cacti", "cactus"),
    ("fungi", "fungus"),
    ("octopi", "octopus"),

    -- Adverbs
    ("quickly", "quick"),
    ("happily", "happy"),
    ("sadly", "sad"),
    ("angrily", "angry"),

    -- Others
    ("is", "be"),
    ("are", "be"),
    ("was", "be"),
    ("were", "be"),
    ("am", "be"),
    ("having", "have"),
    ("has", "have"),
    ("had", "have"),
    ("will", "will"),
    ("would", "would"),
    ("can", "can"),
    ("could", "could"),
    ("should", "should"),
    ("does", "do"),
    ("doing", "do"),

    -- Comparatives and superlatives
    ("taller", "tall"),
    ("tallest", "tall"),
    ("smaller", "small"),
    ("smallest", "small"),

    -- More common words
    ("happiest", "happy"),
    ("interesting", "interesting"),
    ("interested", "interest"),
    ("disappointed", "disappoint"),
    ("disappointing", "disappoint"),

    -- Other useful mappings
    ("let's", "let us"),
    ("can't", "cannot"),
    ("won't", "will not"),
    ("doesn't", "does not"),
    ("don't", "do not"),
    ("isn't", "is not"),
    ("aren't", "are not"),
    ("wasn't", "was not"),
    ("weren't", "were not") ]

-- A simple rule-based lemmatization function
lemmatize :: String -> LemmaDict -> String
lemmatize word dict =
    let normalizedWord = T.unpack . T.toLower $ T.pack word
    in case Map.lookup normalizedWord dict of
        Just lemma -> lemma  -- Return the lemma if found
        Nothing    -> normalizedWord  -- Return the original word if not found

-- Display lemmatized tokens
displayLemmatizedTokens :: [String] -> IO ()
displayLemmatizedTokens lemmatized = do
    putStrLn $ "Lemmatized Token Count: " ++ show (length lemmatized)
    let uniqueLemmatized = List.nub lemmatized  -- Remove duplicates
    putStrLn $ "Unique Lemmatized Tokens: " ++ show (length uniqueLemmatized)
    putStrLn "Lemmatized Tokens: "
    mapM_ putStrLn uniqueLemmatized

