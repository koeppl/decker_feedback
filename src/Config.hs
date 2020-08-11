{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Conduit
import Control.Concurrent (ThreadId)
import Control.Concurrent (killThread)
import Control.Exception (throwIO)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class
import Control.Monad.Logger (MonadLogger(..))
import Control.Monad.Logger
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.ByteString.Char8 as BS
import Data.Monoid ((<>))
import Database.Persist.Sqlite (ConnectionPool, createSqlitePool)
import Database.Persist.Sqlite
import Network.Wai (Middleware)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import System.Environment (lookupEnv)
import Servant

newtype AppT m a = AppT
  { runApp :: ReaderT Config (ExceptT ServerError m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Config
             , MonadError ServerError
             , MonadIO
             )

data Config = Config
  { configPool :: ConnectionPool
  }
