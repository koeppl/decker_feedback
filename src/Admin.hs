{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Admin where

import Relude
import Relude.Extra.Map
import Servant.API
import Servant.Server
import State
import Token

type AuthAPI =
  BasicAuth "Decker Engine admin credentials" User
    :> "login"
    :> Get '[JSON] Token

authAPI :: Proxy AuthAPI
authAPI = Proxy

authServer :: Server AuthAPI
authServer user = liftIO $ calcToken (Just "something")

checkBasicAuth :: EngineState -> BasicAuthCheck User
checkBasicAuth store = BasicAuthCheck $ \basicAuthData ->
  let userDB = stateUserDB store
      login = decodeUtf8 (basicAuthUsername basicAuthData)
      password = decodeUtf8 (basicAuthPassword basicAuthData)
   in case lookup login (users userDB) of
        Nothing -> return NoSuchUser
        Just user -> do
          print $ hashPassword password (salt user)
          if authenticateUser login password userDB
            then do
              makeSessionToken store user
              return (Authorized user)
            else return BadPassword
