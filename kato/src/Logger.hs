{-# LANGUAGE OverloadedStrings #-}

module Logger (logInfo, logError) where

import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.IO (withFile, IOMode(AppendMode), hPutStr)

-- Log information messages
logInfo :: String -> IO ()
logInfo message = logMessage "INFO" message

-- Log error messages
logError :: String -> IO ()
logError message = logMessage "ERROR" message

-- Log messages with timestamps
logMessage :: String -> String -> IO ()
logMessage level message = do
    currentTime <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
    let logEntry = timestamp ++ " [" ++ level ++ "] " ++ message ++ "\n"

    withFile "kato.log" AppendMode $ \handle -> do
        hPutStr handle logEntry

