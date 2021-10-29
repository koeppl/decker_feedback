{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Token where

import Data.Aeson.TH
import Data.Char
import Data.Digest.Pure.MD5
import qualified Data.Text as Text
import Relude hiding (get)
import Servant
import System.Random

-- | Tries to generate a hash a token from the authorization header value. If
-- the header is empty, a random token is returned.
type GetToken = "token" :> Header "Authorization" Text :> Get '[JSON] Token

-- | Calculates the first 9 digits of the MD5 hash of the argument.
hash9 :: Text -> Text
hash9 text = Text.pack $ take 9 $ show $ md5 $ encodeUtf8 text

randomToken :: IO Text
randomToken =
  hash9 . show <$> (getStdRandom random :: IO Word64)

calcToken :: (Maybe Text) -> IO Token
calcToken authorization =
  do
    rnd <- hash9 . show <$> (getStdRandom random :: IO Word64)
    case authorization of
      Just credentials -> return $ Token rnd (Just $ hash9 credentials) Nothing
      Nothing -> return $ Token rnd Nothing Nothing

data Token = Token
  { tokenRandom :: Text,
    tokenAuthorized :: Maybe Text,
    tokenAdmin :: Maybe Text
  }
  deriving (Show)

$( deriveJSON
     defaultOptions {fieldLabelModifier = drop 5 . map toLower}
     ''Token
 )
