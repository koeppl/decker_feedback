{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module State where

import Data.Aeson.TH
import Data.Digest.Pure.SHA
import Data.Yaml
import Database.Persist.Sql (ConnectionPool)
import Relude
import Relude.Extra.Map
import Token

data User = User
  { login :: Text, -- Login name
    hash :: Text, -- SHA256 hash of the password
    salt :: Text, -- Salt that was appended to the password
    decks :: [Text] -- List of owned deck urls
  }
  deriving (Show)

$(deriveJSON defaultOptions ''User)

-- | Run-time database of user credentials
data UserDB = UserDB {users :: Map Text User} deriving (Show)

$(deriveJSON defaultOptions ''UserDB)

-- | Store for admin sessions
type AdminSessions = Map Text User

data EngineState = EngineState
  { stateUserDB :: UserDB,
    -- statePool :: Pool Sqlite.SqlBackend,
    stateSessions :: TVar AdminSessions
  }

data Config = Config
  { userDB :: UserDB,
    adminSessions :: TVar AdminSessions,
    dbPool :: ConnectionPool
  }

loadUserDB :: IO UserDB
loadUserDB = do
  let fileName = "db/users.yaml"
  decodeFileThrow fileName

makeSessionToken :: EngineState -> User -> IO Text
makeSessionToken store user = do
  token <- randomToken
  atomically $ modifyTVar' (stateSessions store) (insert token user)
  return token

makeSessionToken' :: TVar AdminSessions -> User -> IO Text
makeSessionToken' sessions user = do
  token <- randomToken
  atomically $ modifyTVar' sessions (insert token user)
  return token

isAdminUser' :: TVar AdminSessions -> Text -> IO (Maybe User)
isAdminUser' sessions token = do
  lookup token <$> (atomically $ readTVar sessions)

hashPassword :: Text -> Text -> Text
hashPassword password salt =
  toText $ showDigest $ sha256 $ encodeUtf8 (password <> salt)

authenticateUser :: Text -> Text -> UserDB -> Bool
authenticateUser login password (UserDB db) =
  case lookup login db of
    Just (User _ hash salt _) -> hash == hashPassword password salt
    Nothing -> False

authenticateUser' :: Text -> Text -> UserDB -> Maybe User
authenticateUser' login password (UserDB db) =
  case lookup login db of
    Just user@(User _ hash salt _)
      | hash == hashPassword password salt -> return user
    _ -> Nothing
