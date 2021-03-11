{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Notify where

import Auth
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Data.Maybe
import Database.Persist.Sqlite as Sqlite
import Engine
import Model
import Network.Mail.Mime
import Relude
import State
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Pretty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- import Text.Groom

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
  -- unless (isLocalDeck deck) $ do
  let all = toList $ users $ userDB config
  let admins = filter (sendEmailForDeck deck) all
  allComments <- allCommentsForDeck config deck
  mapM_ (notify allComments) admins
  where
    deck = commentDeck comment
    notify allComments admin = do
      let from = Address Nothing "engine@decker.tools"
      let to = Address Nothing (email admin)
      let text = renderCommentText comment
      let html = renderCommentHtml comment allComments
      -- putStrLn "Mails:"
      -- print text
      -- print html
      mail <-
        simpleMail
          to
          from
          "New question in deck"
          (toLazy text)
          (toLazy html)
          []
      renderSendMail mail

renderCommentText :: Comment -> Text
renderCommentText comment =
  let deck = commentDeck comment
      slide = commentSlide comment
      referrer = commentReferrer comment
      link = fromMaybe (deck <> "#" <> slide) referrer
   in "You have one new question on slide:\n\n"
        <> link
        <> "\n\n"
        <> show (commentCreated comment)
        <> "\n\n"
        <> commentMarkdown comment

renderCommentHtml :: Comment -> [Comment] -> Text
renderCommentHtml comment allComments =
  let deck = toString $ commentDeck comment
      slide = toString $ commentSlide comment
      referrer = toString <$> commentReferrer comment
      linkText = deck <> "#" <> slide
   in toText $
        renderHtml $
          H.html $ do
            H.head $ do
              H.title "New Question Email"
              H.style "h1{font-size:1.4em;}h2{font-size:1.2em;}h3,h4,h5,h6{font-size:1em;}"
            H.body $ do
              H.h1 ! A.style "font-size:1.6em;" $ "New Question"
              H.p "You have one new question on slide:"
              case referrer of
                Just href -> H.p $ H.a ! A.href (toValue href) $ H.code $ toHtml linkText
                Nothing -> H.p $ H.code $ toHtml linkText
              H.p $ preEscapedToHtml $ commentHtml comment

-- H.h2 "All questions in the deck:"
-- H.ul $ toHtml $ map (H.li . toHtml . commentHtml) allComments

allCommentsForDeck :: Config -> Text -> IO [Model.Comment]
allCommentsForDeck config deck = do
  let pool = dbPool config
  let req = selectList [CommentDeck ==. deck] [Desc CommentCreated]
  comments :: [Entity Comment] <- runSqlPool req pool
  return $ map entityVal comments
