{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module State where

import Data.Aeson.TH
import Data.Char
import Data.Digest.Pure.SHA
import Data.Pool
import Data.Yaml
import qualified Database.Persist.Sqlite as Sqlite
import Relude
import Relude.Extra.Map
import Servant.API
import Servant.Server
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

loadUserDB :: IO UserDB
loadUserDB = do
  let fileName = "db/users.yaml"
  decodeFileThrow fileName

makeEngineState :: IO EngineState
makeEngineState = do
  users <- loadUserDB
  sessions <- newTVarIO $ fromList []
  return $ EngineState users sessions

makeSessionToken :: EngineState -> User -> IO Text
makeSessionToken store user = do
  token <- randomToken
  atomically $ modifyTVar' (stateSessions store) (insert token user)
  return token

isAdminUser :: EngineState -> Text -> IO (Maybe User)
isAdminUser store token = undefined

hashPassword :: Text -> Text -> Text
hashPassword password salt =
  toText $ showDigest $ sha256 $ encodeUtf8 (password <> salt)

authenticateUser :: Text -> Text -> UserDB -> Bool
authenticateUser login password (UserDB db) =
  case lookup login db of
    Just (User _ hash salt _) -> hash == hashPassword password salt
    Nothing -> False

