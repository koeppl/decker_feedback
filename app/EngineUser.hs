{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EngineUser where

import qualified Data.Map as Map
import Data.Time
import Data.Yaml
import Relude
import State
import System.Environment
import Token
import Auth

main :: IO ()
main = do
  now <- getCurrentTime
  args <- map toText <$> getArgs
  case args of
    [login, password, url, email] -> do
      salt <- randomToken
      let user = User login (hashPassword password salt) salt [url] email
      let users = fromList [(login, user)] :: Map Text User
      putStrLn $ decodeUtf8 $ encode users
    [login, url] -> do
      userDb <- users <$> loadUserDB
      case Map.lookup login userDb >>= isAdminForDeck url of
        Just user -> print $ login <> " is admin for " <> url
        Nothing -> print $ login <> " is NOT admin for " <> url
    _ -> do
      name <- getProgName
      putStrLn $ "usage: " <> name <> " login password deck-url email"
