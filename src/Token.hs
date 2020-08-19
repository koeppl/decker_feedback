{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  = do liftIO $ putStrLn $ "authorization: " <> show authorization
       case authorization of
         Just something -> return $ Token $ hash9 something
         Nothing -> do rnd :: Word64 <- liftIO $ getStdRandom random
                       return $ Token $ hash9 $ show rnd

data Token = Token { tokenText :: Text }
  deriving ( Show )

$( deriveJSON
  defaultOptions { fieldLabelModifier = drop 5 . map toLower }
  ''Token)
