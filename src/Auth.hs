{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Auth where

import Data.ByteString.Base64
import qualified Data.Map as Map
import qualified Data.Text as Text
import Engine
import Network.Wai
import Network.Wai.Middleware.HttpAuth
import Relude
import State
import Token

mkRandomToken :: Handler Token
mkRandomToken = do
  rnd <- liftIO randomToken
  return $ Token rnd Nothing Nothing

mkUserToken :: Text -> Handler Token
mkUserToken creds = do
  rnd <- liftIO randomToken
  let usr = Just $ hash9 creds
  return $ Token rnd usr Nothing

mkAdminToken :: Text -> User -> Handler Token
mkAdminToken creds user = do
  rnd <- liftIO randomToken
  let usr = Just $ hash9 creds
  adm <- liftIO randomToken
  sessions <- asks adminSessions
  liftIO $ atomically $ modifyTVar' sessions (Map.insert adm user)
  return $ Token rnd usr (Just adm)

adminUser :: Maybe Text -> Text -> Handler (Maybe User)
adminUser login deck = do
  db <- users <$> asks userDB
  return $ login >>= flip Map.lookup db >>= isAdminForDeck deck

isAdminUser :: Maybe Text -> Text -> Handler (Maybe User)
isAdminUser (Just token) deck = do
  sessionStore <- asks adminSessions
  admin <- liftIO $ isAdminUser' sessionStore token
  case admin of
    Just user -> return $ isAdminForDeck deck user
    _ -> return Nothing
isAdminUser Nothing _ = return Nothing

-- Decks that are considered local test decks and can be administered by
-- everyone
localDecks = ["http://localhost", "http://0.0.0.0"]

isLocalDeck :: Text -> Bool
isLocalDeck = hasAnyPrefix localDecks

-- Checks if user is admin for the deck. All authenticated users are admin for local test decks.
isAdminForDeck :: Text -> User -> Maybe User
isAdminForDeck deck user =
  if hasAnyPrefix (decks user <> localDecks) deck
    then Just user
    else Nothing

-- Checks if a notification email should be sent to user. Never send for local
-- test decks.
sendEmailForDeck :: Text -> User -> Bool
sendEmailForDeck deck user = hasAnyPrefix (decks user) deck

hasAnyPrefix :: [Text] -> Text -> Bool
hasAnyPrefix prefixes text = any (`Text.isPrefixOf` text) prefixes

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
