{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status404)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (unpack)  -- Import for unpacking ByteString
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeExtension)
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)  -- Import for decoding ByteString to Text
import Servant
import API  -- Import your API module
import Data.Aeson (encode)

-- Function to serve static files
serveStaticFile :: FilePath -> IO Response
serveStaticFile filePath = do
    content <- BL.readFile filePath
    let mimeType = case takeExtension filePath of
            ".html" -> "text/html"
            ".css"  -> "text/css"
            ".js"   -> "application/javascript"
            _       -> "application/octet-stream"
    return $ responseLBS status200 [("Content-Type", mimeType)] content

-- Application handler
app :: Application
app request respond = do
    let path = pathInfo request
    case path of
        [] -> serveStaticFile "static/index.html" >>= respond
        ["app.js"] -> serveStaticFile "static/app.js" >>= respond
        ["styles.css"] -> serveStaticFile "static/styles.css" >>= respond
        _ -> if path == ["tokenize"]
            then handleTokenize request respond
            else do
                let requestedFile = "static" </> (intercalate "/" (map T.unpack path))
                fileExists <- doesFileExist requestedFile
                if fileExists
                    then serveStaticFile requestedFile >>= respond
                    else respond $ responseLBS status404 [("Content-Type", "text/plain")] "404 Not Found"

-- Handle the tokenize API
handleTokenize :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleTokenize request respond = do
    body <- strictRequestBody request  -- Get the request body
    let inputText = decodeUtf8 (BL.toStrict body)  -- Decode the ByteString to Text
    let tokensResponse = tokenizeHandler inputText  -- Call the tokenize handler
    respond $ responseLBS status200 [("Content-Type", "application/json")] (encode tokensResponse)

-- Main function to run the server
main :: IO ()
main = do
    putStrLn "Server running on http://localhost:8080"
    run 8080 app

