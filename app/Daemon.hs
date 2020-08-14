module Daemon where

import Data.Maybe
import System.Environment
import System.Posix.Daemonize

import Engine (daemon)

main :: IO ()
main = do
  userId <- fromMaybe "decker" <$> lookupEnv "DECKER_USER"
  groupId <- fromMaybe "decker" <$> lookupEnv "DECKER_GROUP"
  serviced
    simpleDaemon
      { program = const daemon
      , name = Just "decker"
      , user = Just userId
      , group = Just groupId
      }
