{-# LANGUAGE OverloadedStrings #-}

module Logger (logInfo, logError) where -- Module declaration and exported functions

import Data.Time (getCurrentTime, formatTime, defaultTimeLocale) -- Importing for handling time (pure functions)
import System.IO (withFile, IOMode(AppendMode), hPutStr)         -- Importing I/O utilities for file handling

-- Log information messages (Higher-Order Function)
logInfo :: String -> IO ()
logInfo message = logMessage "INFO" message

-- Log error messages (Higher-Order Function)
logError :: String -> IO ()
logError message = logMessage "ERROR" message

-- Log messages with timestamps (Monads, Immutability, I/O)
logMessage :: String -> String -> IO ()
logMessage level message = do    -- Monad for handling I/O
    currentTime <- getCurrentTime  -- Retrieves current time (Pure function with I/O Monad)
    let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime -- Formats time (Pure function)
    let logEntry = timestamp ++ " [" ++ level ++ "] " ++ message ++ "\n" -- Immutable concatenation of strings

    -- File I/O (Monads)
    withFile "kato.log" AppendMode $ \handle -> do -- Opens file in append mode (I/O Monad)
        hPutStr handle logEntry                    -- Writes log entry to the file (I/O Monad)

