{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Engine where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Relude
import State
import Web.Scotty.Trans

type Error = Text

instance ScottyError Text where
  stringError = toText
  showError = toLazy

newtype EngineM a = EngineM
  { runEngineM :: ReaderT Config (LoggingT IO) a
  }
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadReader Config,
      MonadLogger
    )

type Handler = ActionT Error EngineM
