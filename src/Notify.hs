{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Notify where

import Auth
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Database.Persist.Sql
import Engine
import Model
import Network.Mail.Mime
import Network.URI
import Relude
import State

startNotifier :: Config -> IO ()
startNotifier config = do
  forkIO $ forever (notificationLoop config)
  return ()

notificationLoop :: Config -> IO ()
notificationLoop config = do
  let chan = notificationChannel config
  comment <- atomically $ readTChan chan
  notifyAdminsOfDeck config comment

notify :: Model.Comment -> Handler ()
notify comment = do
  chan <- asks notificationChannel
  atomically $ writeTChan chan comment

notifyAdminsOfDeck :: Config -> Model.Comment -> IO ()
notifyAdminsOfDeck config comment = do
  let all = toList $ users $ userDB config
  unless (isLocalDeck deck) $ do
    let admins = filter (isJust . isAdminForDeck deck) all
    mapM_ notify admins
  where
    deck = commentDeck comment
    slide = commentSlide comment
    notify admin = do
      let url = parseURI $ toString (deck <> "#" <> slide)
      let link = maybe (deck <> "#" <> slide) show url
      let from = Address Nothing "engine@decker.tools"
      let to = Address Nothing (email admin)
      let text =
            "You have one new question on slide:\n\n"
              <> link
              <> "\n\n"
              <> show (commentCreated comment)
              <> "\n\n"
              <> commentMarkdown comment
      let mail = simpleMail' to from "New question in deck" (toLazy text)
      renderSendMail mail
      print mail

allCommentsForDeck :: Config -> Text -> IO [Model.Comment]
allCommentsForDeck config deck = do
  let pool = dbPool config
  let req =
        rawSql
          "select ?? from comment where deck like ? group by deck order by created"
          [PersistText $ deck <> "%"]
  comments :: [Entity Comment] <- runSqlPool req pool
  return $ map entityVal comments
