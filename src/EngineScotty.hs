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
import qualified Data.Text as Text
import qualified Data.Text.Internal.Builder as Text
import Data.Time
import Database.Persist.Sqlite as Sqlite
import Model
import Network.HTTP.Types.Status
import Network.Wai
-- import Network.Wai.EventSource
import Network.Wai.Middleware.RequestLogger
-- import Network.Wai.Middleware.Routed
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
  putStrLn $ show users
  sessions <- newTVarIO $ fromList []
  let config = Config users sessions pool
  cors <- corsWare
  scottyT 8081 (runAction config) (app cors)

runAction :: Config -> EngineM Response -> IO Response
runAction config action =
  runStdoutLoggingT (runReaderT (runEngineM action) config)

type Error = Text

instance ScottyError Text where
  stringError = toText
  showError = toLazy

app :: Middleware -> ScottyT Error EngineM ()
app cors = do
  middleware logStdoutDev
  middleware cors
  middleware $ staticPolicy (addBase "static")
  S.get "/token" getToken
  S.put "/comments" getComments
  S.delete "/comments" deleteComment
  S.post "/comments" postComment
  S.put "/login" loginAdmin
  S.put "/vote" upvoteComment

-- | Responds with a token for the session. If the authorization header is set,
-- authentication by a proxy os assumed. Otherwise the app generates
-- unauthorized.
getToken :: ActionT Error EngineM ()
getToken = do
  value <- fmap toStrict <$> header "Authorization"
  deck <- fromJust . fmap toStrict <$> header "Referer"
  logI $ "getToken from:" <> show deck
  case value of
    Just creds -> do
      -- basicly authorized
      admin <- adminUser (authUser value) deck
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

adminUser :: Maybe Text -> Text -> ActionT Error EngineM (Maybe User)
adminUser login deck = do
  db <- users <$> asks userDB
  return $ login >>= (flip lookup) db >>= isAdminForDeck deck

isAdminUser :: Maybe Text -> Text -> ActionT Error EngineM (Maybe User)
isAdminUser (Just token) deck = do
  sessionStore <- asks adminSessions
  admin <- liftIO $ isAdminUser' sessionStore token
  case admin of
    Just user -> return $ isAdminForDeck deck user
    _ -> return Nothing
isAdminUser Nothing _ = return Nothing

isAdminForDeck :: Text -> User -> Maybe User
isAdminForDeck deck user =
  if any (`Text.isPrefixOf` deck) (decks user)
    then Just user
    else Nothing

type CommentKey = Model.Key Model.Comment

numberOfVotes :: CommentKey -> ActionT Error EngineM Int
numberOfVotes comment = runDb $ count [VoteComment ==. comment]

didPersonVote :: CommentKey -> Key Person -> ActionT Error EngineM Bool
didPersonVote comment voterKey =
  isJust
    <$> runDb
      ( selectFirst
          [VoteComment ==. comment, VoteVoter ==. voterKey]
          []
      )

getComments :: ActionT Error EngineM ()
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
  json $ reverse $ sortOn commentVotes comments
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

type Handler = ActionT Error EngineM

getOrCreatePerson :: Text -> Handler (Entity Person)
getOrCreatePerson token = do
  person <- runDb (selectFirst [PersonToken ==. token] [])
  case person of
    Just person -> return person
    Nothing -> do
      key <- runDb (Sqlite.insert (Person token))
      person <- fromJust <$> runDb (Sqlite.get key)
      return $ Entity key person

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
  case idToken ident of
    Just token -> do
      author <- fmap entityKey <$> runDb (selectFirst [PersonToken ==. token] [])
      comment <- runDb $ Sqlite.get (idKey ident)
      case comment of
        Just comment -> do
          admin <- isAdminUser (idToken ident) (Model.commentDeck comment)
          if isJust author && Model.commentAuthor comment == author || isJust admin
            then do
              logI $ "delete comment with id: " <> show (idKey ident)
              runDb $ Sqlite.deleteWhere [VoteComment ==. idKey ident]
              runDb $ Sqlite.delete (idKey ident)
              status noContent204
            else status forbidden403
        Nothing -> status notFound404
    Nothing -> status forbidden403

-- | Creates and returns an admin token for the provided credentials.
loginAdmin :: ActionT Error EngineM ()
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
          json $ (fromList [("admin", token)] :: Map Text Text)

upvoteComment :: ActionT Error EngineM ()
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
          [VoteComment ==. (entityKey comment), VoteVoter ==. voterKey]
      status ok200
    upVote _ _ _ = do
      status notFound404

logI = lift . logInfoN

-- logW = lift . logWarnN
logE = lift . logErrorN
