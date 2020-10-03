{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EngineScotty where

import Auth
import Commonmark
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Cors
import Data.Maybe
import qualified Data.Text.Internal.Builder as Text
import Data.Time
import Database.Persist.Sqlite as Sqlite
import Model
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Query
import Relude
import Relude.Extra.Map as Map (insert, lookup)
import State
import Token
import View
import Web.Scotty.Trans as S

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
  sessions <- newTVarIO $ fromList []
  let config = Config users sessions pool
  state <- makeEngineState
  cors <- corsWare
  auth <- authWare config
  scottyT 8081 (runAction config state) (app cors auth)

runAction :: Config -> EngineState -> EngineM Response -> IO Response
runAction config state action =
  runStdoutLoggingT (runReaderT (runEngineM action) config)

type Error = Text

instance ScottyError Text where
  stringError = toText
  showError = toLazy

app :: Middleware -> Middleware -> ScottyT Error EngineM ()
app cors auth = do
  middleware logStdoutDev
  middleware cors
  -- middleware auth
  middleware $ staticPolicy (addBase "static")
  S.get "/token" getToken'
  S.put "/comments" getComments
  S.delete "/comments" deleteComment
  S.post "/comments" postComment
  S.put "/login" loginAdmin
  S.put "/upvote" upvoteComment

getToken' :: ActionT Error EngineM ()
getToken' = do
  value <- fmap toStrict <$> header "Authorization"
  case value of
    Just creds -> do
      -- basicly authorized
      admin <- adminUser $ authUser value
      case admin of
        Just user -> do
          -- authorized admin
          mkAdminToken creds user >>= json
        Nothing -> do
          -- just a user
          mkUserToken creds >>= json
    Nothing -> do
      -- not authorized
      mkRandomToken >>= json

mkRandomToken :: ActionT Error EngineM Token
mkRandomToken = do
  rnd <- liftIO randomToken
  return $ Token rnd Nothing Nothing

mkUserToken :: Text -> ActionT Error EngineM Token
mkUserToken creds = do
  rnd <- liftIO randomToken
  let usr = Just $ hash9 creds
  return $ Token rnd usr Nothing

mkAdminToken :: Text -> User -> ActionT Error EngineM Token
mkAdminToken creds user = do
  rnd <- liftIO randomToken
  let usr = Just $ hash9 creds
  adm <- liftIO randomToken
  sessions <- asks adminSessions
  liftIO $ atomically $ modifyTVar' sessions (Map.insert adm user)
  return $ Token rnd usr (Just adm)

adminUser :: Maybe Text -> ActionT Error EngineM (Maybe User)
adminUser login = do
  db <- users <$> asks userDB
  return $ login >>= (flip lookup) db

isAdminUser :: Maybe Text -> ActionT Error EngineM (Maybe User)
isAdminUser (Just token) = do
  sessionStore <- asks adminSessions
  liftIO $ isAdminUser' sessionStore token
isAdminUser Nothing = return Nothing

type CommentKey = Model.Key Model.Comment

numberOfVotes :: CommentKey -> ActionT Error EngineM Int
numberOfVotes comment = runDb $ count [VoteComment ==. comment]

didVote :: CommentKey -> Maybe (Entity Person) -> ActionT Error EngineM Bool
didVote comment voter = case voter of
  Just voter ->
    isJust
      <$> runDb
        ( selectFirst
            [VoteComment ==. comment, VoteVoter ==. (entityKey voter)]
            []
        )
  Nothing -> return False

getComments :: ActionT Error EngineM ()
getComments = do
  logI "GET /comments"
  selector :: Query.Select <- jsonData
  logI $ show selector
  author :: Maybe (Entity Person) <-
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
  admin <- isAdminUser (selectToken selector)
  mapM (toView author admin) list >>= json
  where
    toView author admin entity = do
      let c = entityVal entity
      let ckey = entityKey entity
      votes <- numberOfVotes ckey
      didVote <- didVote ckey author
      return $
        View.Comment
          (entityKey entity)
          (personToken . entityVal <$> author)
          (Model.commentMarkdown c)
          (Model.commentHtml c)
          (Model.commentCreated c)
          votes
          didVote

compileMarkdown :: Text -> Text
compileMarkdown markdown =
  let escaped = toStrict $ Text.toLazyText $ escapeHtml markdown
   in case commonmark "stdin" escaped of
        Left _ -> "<p>" <> escaped <> "</p>"
        Right (html :: Html ()) -> toStrict $ renderHtml html

postComment :: ActionT Error EngineM ()
postComment = do
  logI "GET /comments"
  cdata <- jsonData
  logI $ show $ cdata
  now <- liftIO $ getCurrentTime
  author <- case commentToken cdata of
    Just token -> do
      key <-
        fmap entityKey
          <$> runDb (selectFirst [PersonToken ==. token] [])
      case key of
        Just key -> return $ Just key
        Nothing -> Just <$> runDb (Sqlite.insert (Person token))
    Nothing -> return Nothing
  key <-
    runDb $
      Sqlite.insert $
        Model.Comment
          (Query.commentMarkdown cdata)
          (compileMarkdown (Query.commentMarkdown cdata))
          author
          (Query.commentDeck cdata)
          (Query.commentSlide cdata)
          now
  logI $ "insert comment with id: " <> show key
  status ok200

deleteComment :: ActionT Error EngineM ()
deleteComment = do
  ident <- jsonData
  logI $ show ident
  case idToken ident of
    Just token -> do
      author <- fmap entityKey <$> runDb (selectFirst [PersonToken ==. token] [])
      comment <- runDb $ Sqlite.get (idKey ident)
      sessions <- asks adminSessions
      admin <- liftIO $ isAdminUser' sessions token
      case comment of
        Just comment ->
          if isJust author && Model.commentAuthor comment == author || isJust admin
            then do
              logI $ "delete comment with id: " <> show (idKey ident)
              runDb $ Sqlite.delete (idKey ident)
              status noContent204
            else status forbidden403
        Nothing -> status notFound404
    Nothing -> status forbidden403

-- | Creates and returns an admin token for the provided credentials.
loginAdmin :: ActionT Error EngineM ()
loginAdmin = do
  creds <- jsonData
  sessions <- asks adminSessions
  udb <- asks userDB
  case authenticateUser' (credLogin creds) (credPassword creds) udb of
    Just user -> do
      token <- liftIO $ makeSessionToken' sessions user
      json $ (fromList [("admin", token)] :: Map Text Text)
    Nothing -> status forbidden403

upvoteComment :: ActionT Error EngineM ()
upvoteComment = do
  vote <- jsonData
  let commentId = Query.voteComment vote
      voterToken = Query.voteVoter vote
      up = Query.voteUp vote
  comment <- fmap (Entity commentId) <$> runDb (Sqlite.get commentId)
  voter <- runDb (Sqlite.selectFirst [PersonToken ==. voterToken] [])
  did <- didVote commentId voter
  upVote up comment voter did
  where
    upVote True (Just comment) (Just voter) False = do
      runDb $ Sqlite.insert (Model.Vote (entityKey comment) (entityKey voter))
      status ok200
    upVote False (Just comment) (Just voter) True = do
      runDb $
        Sqlite.deleteWhere
          [VoteComment ==. (entityKey comment), VoteVoter ==. (entityKey voter)]
      status ok200
    upVote _ _ _ _ = do
      status notFound404

logI = lift . logInfoN
