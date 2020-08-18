{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Token where

import Relude hiding ( get )

import Servant
import Servant.API
import Servant.Docs
import Servant.Server

type GetToken = "token" :> Header "Authorization" Text :> Get '[JSON] Text

getToken :: (Maybe Text) -> Handler Text
getToken authorization
  = do liftIO $ putStrLn $ "authorization: " <> show authorization
       return "SOMETOKEN"
