{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EngineUser where

import Data.Yaml
import Relude
import State
import System.Environment
import Token

main :: IO ()
main = do
  args <- map toText <$> getArgs
  case args of
    [login, password, url] -> do
      salt <- randomToken
      let user = User login (hashPassword password salt) salt [url]
      let users = fromList [(login, user)] :: Map Text User
      putStrLn $ decodeUtf8 $ encode users
    _ -> do
      name <- getProgName
      putStrLn $ "usage: " <> name <> " login password deck-url"
