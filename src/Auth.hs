{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Auth where

import Data.ByteString.Base64
import qualified Data.Text as Text
import Network.Wai
import Network.Wai.Middleware.HttpAuth
import Relude
import State

-- | Creates a middleware that uses Basic Auth to authenticate all requests to
-- to the login endpoint.
authWare :: Config -> IO Middleware
authWare config = do
  let settings =
        "Be the decker engine admin, if you dare!"
          { authIsProtected = checkProtected
          }
  return $ basicAuth (checkAdmin config) settings

checkAdmin :: Config -> ByteString -> ByteString -> IO Bool
checkAdmin config user password = do
  putStrLn $ "Login attempt: " <> show user 
  return $
    authenticateUser
      (decodeUtf8 user)
      (decodeUtf8 password)
      (userDB config)

checkProtected :: Request -> IO Bool
checkProtected request = do
  return $ pathInfo request == ["login"]

-- | Decodes the Authorization header value and retuns the authorized user
authUser :: Maybe Text -> Maybe Text
authUser header = do
  value <- header
  let ["Basic", encoded] = Text.splitOn " " value
  viaNonEmpty head $ Text.splitOn ":" $ decodeUtf8 $ decodeLenient $ encodeUtf8 encoded
