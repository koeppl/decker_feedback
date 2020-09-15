{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Model where

import Config

import Control.Monad.Reader ( MonadIO, MonadReader, asks, liftIO )

import Data.Text ( Text )
import Data.Time.Clock

import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH

import Network.URI ( URI )

import Relude

import Uri

share
  [ mkPersist sqlSettings, mkMigrate "migrateAll" ]
  [persistLowerCase|
Person json
  token Text
  UniquePersonToken token
  deriving Show Eq
Comment json
  markdown Text
  html Text
  author PersonId Maybe
  deck Text
  slide Text
  created UTCTime default=CURRENT_TIME
  deriving Show Eq
|]

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: ( MonadReader Config m, MonadIO m ) => SqlPersistT IO b -> m b
runDb query
  = do pool <- asks configPool
       liftIO $ runSqlPool query pool
