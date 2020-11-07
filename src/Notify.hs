{-# LANGUAGE BlockArguments #-}
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
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Pretty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Groom

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
    let admins = filter (sendEmailForDeck deck) all
    mapM_ notify admins
  where
    deck = commentDeck comment
    slide = commentSlide comment
    notify :: User -> IO ()
    notify admin = do
      let url = parseURI $ toString (deck <> "#" <> slide)
      let link = maybe (deck <> "#" <> slide) show url
      let from = Address Nothing "engine@decker.tools"
      let to = Address Nothing (email admin)
      let text = renderCommentText comment link
      let html = renderCommentHtml comment link
      mail <-
        simpleMail
          to
          from
          "New question in deck"
          (toLazy text)
          (toLazy html)
          []
      renderSendMail mail
      putStrLn $ groom mail

renderCommentText :: Comment -> Text -> Text
renderCommentText comment link =
  "You have one new question on slide:\n\n"
    <> link
    <> "\n\n"
    <> show (commentCreated comment)
    <> "\n\n"
    <> commentMarkdown comment

renderCommentHtml :: Comment -> Text -> Text
renderCommentHtml comment link =
  toText $
    renderHtml $
      H.html $ do
        H.head $ do
          H.title "New Question Email"
          H.style ""
        H.body $ do
          H.h1 "New Question"
          H.p "You have one new question on slide:"
          H.p $ H.a ! A.href (toValue link) $ H.code $ toHtml link
          H.p $ preEscapedToHtml $ commentHtml comment

allCommentsForDeck :: Config -> Text -> IO [Model.Comment]
allCommentsForDeck config deck = do
  let pool = dbPool config
  let req =
        rawSql
          "select ?? from comment where deck like ? group by deck order by created"
          [PersistText $ deck <> "%"]
  comments :: [Entity Comment] <- runSqlPool req pool
  return $ map entityVal comments
