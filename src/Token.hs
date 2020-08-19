{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Token where

import Data.Aeson.TH
import Data.Char
import Data.Digest.Pure.MD5
import qualified Data.Text as Text

import Relude hiding ( get )

import Servant
import Servant.API
import Servant.Docs
import Servant.Server

import System.Random

type GetToken = "token" :> Header "Authorization" Text :> Get '[JSON] Token

hash9 :: Text -> Text
hash9 text = Text.pack $ take 9 $ show $ md5 $ encodeUtf8 text

getToken :: (Maybe Text) -> Handler Token
getToken authorization
  = do rnd <- hash9 . show <$> (liftIO $ getStdRandom random :: Handler Word64)
       case authorization of
         Just credentials -> return $ Token rnd (Just $ hash9 credentials)
         Nothing -> return $ Token rnd Nothing

data Token = Token { tokenRandom :: Text, tokenAuthorized :: Maybe Text }
  deriving ( Show )

$( deriveJSON
  defaultOptions { fieldLabelModifier = drop 5 . map toLower }
  ''Token)
