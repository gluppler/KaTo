{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import Servant
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (ToJSON, object, (.=))  -- Import (.=) operator here
import qualified Data.Aeson as Aeson

-- Define the API type
type API = "tokenize" :> ReqBody '[PlainText] Text :> Post '[JSON] TokenResponse

-- Define the response type for tokenization
data TokenResponse = TokenResponse
    { tokens :: [Text]
    } deriving (Generic)

-- Make TokenResponse an instance of ToJSON
instance ToJSON TokenResponse where
    toJSON (TokenResponse tokens) = object ["tokens" .= tokens]  -- Now this will work

-- Function to tokenize the input string
tokenize :: Text -> [Text]
tokenize input = filter (not . T.null) (T.words input)  -- Split on whitespace and filter out empty tokens

-- Tokenization handler
tokenizeHandler :: Text -> TokenResponse
tokenizeHandler input = TokenResponse (tokenize input)

-- Proxy for the API
api :: Proxy API
api = Proxy

