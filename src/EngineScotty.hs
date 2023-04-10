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

import Conduit (MonadUnliftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan)
import Control.Concurrent.STM (newTChan)
import Control.Monad.Logger
import Cors
import Data.Acquire (with)
import Data.Maybe
import Data.Pool (Pool)
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
import System.Directory (createDirectoryIfMissing)
import View
import Web.Scotty.Trans as S

connectDB :: IO ConnectionPool
connectDB = runStdoutLoggingT $ do
  pool <- createSqlitePool "db/engine.sqlite3" 10
  -- runSqlPoolNoTransaction (rawExecute "PRAGMA foreign_keys=OFF" []) pool
  runSqlPool (runMigration migrateAll) pool
  -- runSqlPoolNoTransaction (rawExecute "PRAGMA foreign_keys=ON" []) pool
  return pool

-- runSqlPoolNoTransaction ::
--   forall backend m a.
--   (MonadUnliftIO m, BackendCompatible SqlBackend backend) =>
--   ReaderT backend m a ->
--   Pool backend ->
--   m a
-- runSqlPoolNoTransaction r pconn = with (unsafeAcquireSqlConnFromPool pconn) $ runReaderT r

runDb req = do
  pool <- asks dbPool
  liftIO $ runSqlPool req pool

engine :: IO ()
engine = do
  createDirectoryIfMissing True "db"
  -- createDirectoryIfMissing True "log"
  pool <- connectDB
  users <- loadUserDB
  sessions <- newTVarIO $ fromList []
  notificationChannel <- atomically newTChan
  let config = Config users sessions notificationChannel pool
  cors <- corsWare
  startNotifier config
  scottyT 8081 (runAction config) (app cors)

-- | Installs a threaded log writer and runs the action in the engine monad.
runAction :: Config -> EngineM Response -> IO Response
runAction config action = do
  -- logChan <- newChan
  -- logWriter logChan
  -- runChanLoggingT logChan (filterLogger loggingFilter (runReaderT (runEngineM action) config))
  -- runFileLoggingT "log/engine.log" (filterLogger loggingFilter (runReaderT (runEngineM action) config))
  runStdoutLoggingT (filterLogger loggingFilter (runReaderT (runEngineM action) config))

logWriter :: Chan LogLine -> IO ()
logWriter chan = do
  _ <- forkIO $ runFileLoggingT "log/engine.log" $ unChanLoggingT chan
  return ()

loggingFilter :: LogSource -> LogLevel -> Bool
loggingFilter source level =
  level
    `elem` [ LevelDebug,
             LevelInfo,
             LevelWarn,
             LevelError
           ]

app :: Middleware -> ScottyT Error EngineM ()
app cors = do
  middleware logStdoutDev
  middleware cors
  middleware $ staticPolicy (addBase "static")
  S.get "/token" getToken
  S.put "/comments" getComments
  S.post "/comments" postOrUpdateComment
  S.delete "/comments" deleteComment
  S.put "/login" loginAdmin
  S.put "/vote" upvoteComment
  S.post "/answers" postAnswer
  S.delete "/answers" deleteAnswer

-- | Responds with a token for the session. If the authorization header is set,
-- authentication by a proxy os assumed. Otherwise the app generates
-- unauthorized.
getToken :: Handler ()
getToken = do
  tokenUser <- fmap toStrict <$> header "X-Token-Subject" -- caddy security JWT sets this
  logI $ "X-Token-Subject: " <> show (fromMaybe "" tokenUser)
  case tokenUser of
    Just name -> do
      logI "generating user token"
      mkUserToken name >>= json
    Nothing -> do
      logI "generating anonymous token"
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
        Sqlite.selectList
          [CommentDeck ==. selectDeck selector]
          [Asc CommentCreated]
  comments <- mapM (toView (entityKey <$> user)) list
  json $ sortOn (Down . commentVotes) comments
  where
    toViewAnswer answer =
      let key = entityKey answer
          val = entityVal answer
       in View.Answer
            key
            (Model.answerMarkdown val)
            (compileMarkdown <$> Model.answerMarkdown val)
            (Model.answerLink val)
            (Model.answerCreated val)
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
      answers <- runDb $ Sqlite.selectList [AnswerComment ==. ckey] [Asc AnswerCreated]
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
          (map toViewAnswer answers)

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
  logI "Update or post comment: "
  cdata <- jsonData
  logI $ show cdata
  case Query.commentId cdata of
    Just _ -> updateComment cdata
    Nothing -> postComment cdata

updateComment :: Query.CommentData -> Handler ()
updateComment cdata = do
  let markdown = fromMaybe "" (Query.commentMarkdown cdata)
  let id = fromJust $ Query.commentId cdata
  let token = Query.commentToken cdata
  let compiled = compileMarkdown markdown
  canDel <- canDelete token id
  if canDel
    then do
      runDb $ Sqlite.update id [CommentMarkdown =. markdown, CommentHtml =. compiled]
      logI $ "Updating comment: " <> show id
      json id
    else do
      logE $ "Cannot update comment: "
      logE $ show cdata
      admin <- isAdminUser token (Query.commentDeck cdata)
      logE $ "Admin: " <> show admin

      status forbidden403

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
  let markdown = fromMaybe "" (Query.commentMarkdown cdata)
  let comment =
        Model.Comment
          markdown
          (compileMarkdown markdown)
          author
          (Query.commentLocation cdata)
          deck
          (Query.commentSlide cdata)
          now
  key <- runDb $ Sqlite.insert comment
  notify comment
  logI $ "Creating comment: " <> show key
  json key

canDelete :: Maybe Text -> Key Model.Comment -> Handler Bool
canDelete token key =
  case token of
    Just token -> do
      author <- fmap entityKey <$> runDb (selectFirst [PersonToken ==. token] [])
      comment <- runDb $ Sqlite.get key
      answers <- runDb $ Sqlite.count [AnswerComment ==. key]
      case comment of
        Just comment -> do
          admin <- isAdminUser (Just token) (Model.commentDeck comment)
          return $
            isJust author
              && Model.commentAuthor comment == author
              && answers == 0 || isJust admin
        Nothing -> return False
    Nothing -> return False

deleteComment :: Handler ()
deleteComment = do
  ident <- jsonData
  let id = idKey ident
  let token = idToken ident
  delete <- canDelete token id
  if delete
    then do
      logI $ "Delete comment with id: " <> show id
      runDb $ Sqlite.deleteWhere [VoteComment ==. id]
      runDb $ Sqlite.deleteWhere [AnswerComment ==. id]
      runDb $ Sqlite.delete id
      status noContent204
    else status forbidden403

-- | Creates and returns an admin token for the provided credentials.
loginAdmin :: Handler ()
loginAdmin = do
  creds <- jsonData
  let deck = credDeck creds
  sessions <- asks adminSessions
  udb <- asks userDB
  let admin = authenticateUser' (credLogin creds) (credPassword creds) udb
  case admin of
    Nothing -> logE $ "Authentication failed for: " <> credLogin creds
    Just admin -> do
      case isAdminForDeck deck admin of
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
          logI $ "Because: " <> show (decks admin)
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

canDeleteAnswer' :: Maybe Text -> Key Model.Answer -> Handler Bool
canDeleteAnswer' token key = return True

canDeleteAnswer :: Maybe Text -> Key Model.Answer -> Handler Bool
canDeleteAnswer token key =
  case token of
    Just token -> do
      author <- fmap entityKey <$> runDb (selectFirst [PersonToken ==. token] [])
      answer <- runDb $ Sqlite.get key
      case answer of
        Just answer -> do
          comment <- runDb $ Sqlite.get (Model.answerComment answer)
          case comment of
            Just comment -> do
              admin <- isAdminUser (Just token) (Model.commentDeck comment)
              return $ isJust author && Model.commentAuthor comment == author || isJust admin
            Nothing -> return False
        Nothing -> return False
    Nothing -> return False

deleteAnswer :: Handler ()
deleteAnswer = do
  ident <- jsonData
  let key = toSqlKey (eidKey ident) :: Key Model.Answer
  let token = eidToken ident
  delete <- canDeleteAnswer token key
  if delete
    then do
      logI $ "Delete answer with id: " <> show key
      runDb $ Sqlite.delete key
      status noContent204
    else status forbidden403

postAnswer :: Handler ()
postAnswer = do
  answer <- jsonData
  let commentId = Query.answerComment answer
  comment <- runDb (Sqlite.get commentId)
  case comment of
    Just comment -> do
      let markdown = Query.answerMarkdown answer
      let link = Query.answerLink answer
      let token = Query.answerToken answer
      now <- liftIO getCurrentTime
      admin <- isAdminUser (Just token) (Model.commentDeck comment)
      case admin of
        Just user -> do
          runDb $ Sqlite.insert (Model.Answer commentId markdown link now)
          status ok200
        Nothing -> status forbidden403
    Nothing -> status notFound404

logI :: Text -> ActionT Error EngineM ()
logI = lift . logInfoN

logD :: Text -> ActionT Error EngineM ()
logD = lift . logDebugN

logW :: Text -> ActionT Error EngineM ()
logW = lift . logWarnN

-- logW = lift . logWarnN
logE :: Text -> ActionT Error EngineM ()
logE = lift . logErrorN
