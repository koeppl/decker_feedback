{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model where

import Data.Time.Clock
import Database.Persist.Sql
import Database.Persist.TH
import Network.URI
import Relude
import Uri ()

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
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
Answer
  comment CommentId
  markdown Text Maybe
  link URI Maybe
  created UTCTime default=CURRENT_TIME
Vote 
  comment CommentId
  voter PersonId
|]
