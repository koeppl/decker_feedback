{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Engine where

import Control.Monad.Logger
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock
import Database.Persist
import Database.Persist.Sqlite
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Relude hiding (group)
import Servant
import Servant.JS
import Servant.JS.Vanilla
import System.Directory
import System.Environment
import System.FilePath

import API
import Model

app :: Application
app = simpleCors $ serve deckerAPI deckerServer

daemon :: IO ()
daemon = do
  baseUrl <- fromMaybe "" <$> lookupEnv "DECKER_BASE_URL"
  rootDir <- fromMaybe "." <$> lookupEnv "DECKER_ROOT_DIR"
  setCurrentDirectory rootDir
  Text.writeFile ("static/decker.js") $
    addExport $
    jsForAPI
      jsAPI
      (vanillaJSWith defCommonGeneratorOptions {urlPrefix = toText baseUrl})
  saveDocs "static/doc.md"
  runSqlite "db/engine.db" $ do runMigration migrateAll
  run 8081 app

addExport :: Text -> Text
addExport = Text.unlines . map insert . Text.lines
  where
    insert line =
      if "var " `Text.isPrefixOf` line
        then "export " <> line
        else line

mock :: ReaderT SqlBackend IO (Key Comment)
mock = do
  now <- liftIO getCurrentTime
  p1 <- insert $ Person "person1"
  p2 <- insert $ Person "person2"
  insert $
    Comment
      "What is the purpose of this stuff? Nobody ever needs to know this."
      "cgg"
      "intro-slide"
      (Just p1)
      now
  insert $ Comment "May I go to the bathroom?" "cgg" "intro-slide" (Just p1) now
  insert $ Comment "Wat?" "cgg" "intro-slide" (Just p2) now
  insert $
    Comment
      "Fucking hell, this is really anonymous! Keep on swearing."
      "cgg"
      "slide-2"
      Nothing
      now
