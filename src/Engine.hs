{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Engine where

import API
import Admin
import Control.Monad.Logger
import Data.Aeson
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock
import Database.Persist
import Database.Persist.Sqlite
import Model
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Query
import Relude hiding (group)
import Servant
import Servant.JS
import Servant.JS.Vanilla
import State
import System.Directory
import System.Environment
import System.FilePath

-- | Starts the server. If policy is Nothing, no CORS headers are served. This
-- mode is meant to be used behind an Apache proxy server that handles the
-- generation of CORS headers. Otherwise, the list of CORS origin URLs is
-- parsed and passed to the CORS middleware (with credentials enabled).
app :: Maybe CorsResourcePolicy -> EngineState -> Application
app policy authStore =
  case policy of
    Just policy -> corsWare policy $ serveWithContext deckerAPI context deckerServer
    Nothing -> serveWithContext deckerAPI context deckerServer
  where
    context = checkBasicAuth authStore :. EmptyContext

-- | Transform a comma separated string of origin URLs into a list for the cors
-- middleware.
readOrigins :: String -> Maybe ([Origin], Bool)
readOrigins line =
  case map Text.strip $ Text.splitOn "," $ toText line of
    ["*"] -> Nothing
    list -> Just (map encodeUtf8 list, True)

corsPolicy :: Maybe ([Origin], Bool) -> CorsResourcePolicy
corsPolicy origins =
  CorsResourcePolicy
    { corsOrigins = origins,
      corsMethods = ["GET", "HEAD", "PUT", "POST", "DELETE"],
      corsRequestHeaders = ["Authorization", "Content-Type"],
      corsExposedHeaders = Just ["Content-Type"],
      corsMaxAge = Nothing,
      corsVaryOrigin = False,
      corsRequireOrigin = False,
      corsIgnoreFailures = False
    }

corsWare :: CorsResourcePolicy -> Middleware
corsWare policy = cors (const $ Just policy)

daemon :: IO ()
daemon = do
  origins <- lookupEnv "DECKER_CORS_ORIGINS"
  putStrLn $ "CORS origins: " <> fromMaybe "<unset>" origins
  -- saveDocs "static/doc.md"
  runSqlite "db/engine.db" $ do runMigration migrateAll
  let policy = corsPolicy . readOrigins <$> origins
  if (isJust policy)
    then putStrLn $ "CORS origins: " <> show (corsOrigins <$> policy)
    else putStrLn "No CORS"
  let settings = setPort 8081 $ setOnException cryOut $ defaultSettings
  -- runSettings settings (app policy)
  authStore <- makeEngineState
  run 8081 (app policy authStore)

cryOut :: Maybe Request -> SomeException -> IO ()
cryOut req err = do
  print req
  print err
  print ""
