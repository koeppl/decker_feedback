{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EngineScotty where

import Auth
import Commonmark
-- import Network.Wai.EventSource

-- import Network.Wai.Middleware.Routed

import Control.Concurrent.STM (newTChan)
import Control.Monad.Logger
import Cors
import Data.Maybe
import qualified Data.Text.Internal.Builder as Text
import Data.Time
import Database.Persist.Sqlite as Sqlite
import Engine
import Model
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Notify
import Query
import Relude
import State
import View
import Web.Scotty.Trans as S

connectDB :: IO ConnectionPool
connectDB = runStdoutLoggingT $ do
  pool <- createSqlitePool "db/engine.sqlite3" 10
  runSqlPool (runMigration migrateAll) pool
  return pool

runDb req = do
  pool <- asks dbPool
  liftIO $ runSqlPool req pool

engine :: IO ()
engine = do
  pool <- connectDB
  users <- loadUserDB
  print users
  sessions <- newTVarIO $ fromList []
  notificationChannel <- atomically newTChan
  let config = Config users sessions notificationChannel pool
  cors <- corsWare
  startNotifier config
  scottyT 8081 (runAction config) (app cors)

runAction :: Config -> EngineM Response -> IO Response
runAction config action =
  runStdoutLoggingT (runReaderT (runEngineM action) config)

app :: Middleware -> ScottyT Error EngineM ()
app cors = do
  middleware logStdoutDev
  middleware cors
  middleware $ staticPolicy (addBase "static")
  S.get "/token" getToken
  S.put "/comments" getComments
  S.delete "/comments" deleteComment
  S.post "/comments" postOrUpdateComment
  S.put "/login" loginAdmin
  S.put "/vote" upvoteComment

-- | Responds with a token for the session. If the authorization header is set,
-- authentication by a proxy os assumed. Otherwise the app generates
-- unauthorized.
getToken :: Handler ()
getToken = do
  value <- fmap toStrict <$> header "Authorization"
  deck <- toStrict . fromJust <$> header "Referer"
  logI $ "getToken from:" <> show deck
  case value of
    Just creds -> do
      -- basicly authorized
      admin <- adminUser (authUser value) deck
      case admin of
        Just user ->
          mkAdminToken creds user >>= json
        Nothing ->
          mkUserToken creds >>= json
    Nothing ->
      mkRandomToken >>= json

type CommentKey = Model.Key Model.Comment

numberOfVotes :: CommentKey -> Handler Int
numberOfVotes comment = runDb $ count [VoteComment ==. comment]

didPersonVote :: CommentKey -> Key Person -> Handler Bool
didPersonVote comment voterKey =
  isJust
    <$> runDb
      ( selectFirst
          [VoteComment ==. comment, VoteVoter ==. voterKey]
          []
      )

getComments :: Handler ()
getComments = do
  logI "GET /comments"
  selector :: Query.Select <- jsonData
  logI $ show selector
  user :: Maybe (Entity Person) <-
    case selectToken selector of
      Just token -> runDb (selectFirst [PersonToken ==. token] [])
      Nothing -> return Nothing
  list :: [Entity Model.Comment] <- case selectSlide selector of
    Just slideId ->
      runDb $
        selectList
          [ CommentDeck ==. selectDeck selector,
            CommentSlide ==. slideId
          ]
          [Asc CommentCreated]
    Nothing ->
      runDb $
        selectList
          [CommentDeck ==. selectDeck selector]
          [Asc CommentCreated]
  comments <- mapM (toView (entityKey <$> user)) list
  json $ sortOn (Down . commentVotes) comments
  where
    toView user entity = do
      let c = entityVal entity
      let ckey = entityKey entity
      votes <- numberOfVotes ckey
      didVote <- case user of
        Just key -> didPersonVote ckey key
        Nothing -> return False
      author <-
        case Model.commentAuthor c of
          Just authorId -> runDb $ Sqlite.get authorId
          Nothing -> return Nothing
      return $
        View.Comment
          (entityKey entity)
          (Model.personToken <$> author)
          (Model.commentMarkdown c)
          (Model.commentHtml c)
          (Model.commentCreated c)
          (Model.commentSlide c)
          votes
          didVote

compileMarkdown :: Text -> Text
compileMarkdown markdown =
  let escaped = toStrict $ Text.toLazyText $ escapeHtml markdown
   in case commonmark "stdin" escaped of
        Left _ -> "<p>" <> escaped <> "</p>"
        Right (html :: Html ()) -> toStrict $ renderHtml html

getOrCreatePerson :: Text -> Handler (Entity Person)
getOrCreatePerson token = do
  person <- runDb (selectFirst [PersonToken ==. token] [])
  case person of
    Just person -> return person
    Nothing -> do
      key <- runDb (Sqlite.insert (Person token))
      person <- fromJust <$> runDb (Sqlite.get key)
      return $ Entity key person

postOrUpdateComment :: Handler ()
postOrUpdateComment = do
  cdata <- jsonData
  logI $ show cdata
  -- If a commentId is given, update existing, else create new one
  update <-
    case Query.commentId cdata of
      Just cId -> canDelete (commentToken cdata) cId
      Nothing -> return False
  if update
    then updateComment cdata
    else postComment cdata

updateComment :: Query.CommentData -> Handler ()
updateComment cdata = do
  logI $ "Updating comment: " <> show (Query.commentId cdata)
  runDb $
    Sqlite.update
      (fromJust $ Query.commentId cdata)
      [ CommentMarkdown =. Query.commentMarkdown cdata,
        CommentHtml =. compileMarkdown (Query.commentMarkdown cdata),
        CommentAnswered =. Query.commentAnswered cdata
      ]

postComment :: Query.CommentData -> Handler ()
postComment cdata = do
  now <- liftIO getCurrentTime
  author <- case commentToken cdata of
    Just token -> do
      key <- fmap entityKey <$> runDb (selectFirst [PersonToken ==. token] [])
      case key of
        Just key -> return $ Just key
        Nothing -> Just <$> runDb (Sqlite.insert (Person token))
    Nothing -> return Nothing
  let deck = Query.commentDeck cdata
  let comment =
        Model.Comment
          (Query.commentMarkdown cdata)
          (compileMarkdown (Query.commentMarkdown cdata))
          author
          deck
          (Query.commentSlide cdata)
          now
          Nothing
  key <- runDb $ Sqlite.insert comment
  notify comment
  logI $ "Creating comment: " <> show key
  status ok200

canDelete :: Maybe Text -> Key Model.Comment -> Handler Bool
canDelete token key = do
  case token of
    Just token -> do
      author <- fmap entityKey <$> runDb (selectFirst [PersonToken ==. token] [])
      comment <- runDb $ Sqlite.get key
      case comment of
        Just comment -> do
          admin <- isAdminUser (Just token) (Model.commentDeck comment)
          return $ isJust author && Model.commentAuthor comment == author || isJust admin
        Nothing -> return False
    Nothing -> return False

deleteComment :: Handler ()
deleteComment = do
  ident <- jsonData
  delete <- canDelete (idToken ident) (idKey ident)
  if delete
    then do
      logI $ "Delete comment with id: " <> show (idKey ident)
      runDb $ Sqlite.deleteWhere [VoteComment ==. idKey ident]
      runDb $ Sqlite.delete (idKey ident)
      status noContent204
    else status forbidden403

-- | Creates and returns an admin token for the provided credentials.
loginAdmin :: Handler ()
loginAdmin = do
  -- deck <- fromJust . fmap toStrict <$> header "Referer"
  creds <- jsonData
  let deck = credDeck creds
  sessions <- asks adminSessions
  udb <- asks userDB
  let admin = authenticateUser' (credLogin creds) (credPassword creds) udb
  case admin of
    Nothing -> logE $ "Authentication failed for: " <> credLogin creds
    Just admin -> do
      let forDeck = isAdminForDeck deck admin
      case forDeck of
        Nothing -> do
          logE $
            "Admin: "
              <> login admin
              <> " not authorized for: "
              <> deck
              <> " "
              <> show (decks admin)
          status forbidden403
        Just user -> do
          logI $ "Login succeeded for: " <> show (credLogin creds) <> " on: " <> show deck
          token <- liftIO $ makeSessionToken' sessions user
          json (fromList [("admin", token)] :: Map Text Text)

upvoteComment :: Handler ()
upvoteComment = do
  vote <- jsonData
  let commentId = Query.voteComment vote
      voterToken = Query.voteVoter vote
  comment <- fmap (Entity commentId) <$> runDb (Sqlite.get commentId)
  voterKey <- do
    key <- fmap entityKey <$> runDb (Sqlite.selectFirst [PersonToken ==. voterToken] [])
    case key of
      Just key -> return key
      Nothing -> runDb $ Sqlite.insert $ Person voterToken
  did <- didPersonVote commentId voterKey
  upVote comment voterKey did
  where
    upVote (Just comment) voterKey False = do
      runDb $ Sqlite.insert (Model.Vote (entityKey comment) voterKey)
      status ok200
    upVote (Just comment) voterKey True = do
      runDb $
        Sqlite.deleteWhere
          [VoteComment ==. entityKey comment, VoteVoter ==. voterKey]
      status ok200
    upVote _ _ _ =
      status notFound404

logI = lift . logInfoN

-- logW = lift . logWarnN
logE = lift . logErrorN
