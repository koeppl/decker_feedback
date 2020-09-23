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
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class
import Cors
import Data.Maybe
import qualified Data.Text.Internal.Builder as Text
import Data.Time
import Database.Persist.Sqlite as Sqlite
import Model
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Query
import Relude
import Relude.Extra.Map (lookup)
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
  middleware auth
  middleware $ staticPolicy (addBase "static")
  S.get "/token" getToken'
  S.put "/comments" getComments
  S.delete "/comments" deleteComment
  S.post "/comments" postComment
  S.get "/login" loginAdmin

getToken' :: ActionT Error EngineM ()
getToken' = do
  value <- fmap toStrict <$> header "Authorization"
  token <- liftIO $ calcToken value
  logI $ "" <> show token
  json token

getComments :: ActionT Error EngineM ()
getComments = do
  logI "GET /comments"
  selector :: Query.Select <- jsonData
  logI $ show selector
  author :: Maybe (Key Person) <-
    case selectToken selector of
      Just token -> do
        fmap entityKey
          <$> runDb (selectFirst [PersonToken ==. token] [])
      Nothing -> return Nothing
  list :: [Entity Model.Comment] <- case selectSlide selector of
    Just slideId ->
      runDb $
        selectList
          [ CommentDeck ==. selectDeck selector,
            CommentSlide ==. slideId
          ]
          [Desc CommentCreated]
    Nothing ->
      runDb $
        selectList
          [CommentDeck ==. selectDeck selector]
          [Desc CommentCreated]
  json $ map (toView author) list
  where
    toView a e =
      let c = entityVal e
       in View.Comment
            (Model.commentMarkdown c)
            (Model.commentHtml c)
            (Model.commentCreated c)
            ( if isJust a && commentAuthor c == a
                then Just (entityKey e)
                else Nothing
            )

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
        Nothing -> Just <$> runDb (insert (Person token))
    Nothing -> return Nothing
  key <-
    runDb $
      insert $
        Model.Comment
          (Query.commentMarkdown cdata)
          (compileMarkdown (Query.commentMarkdown cdata))
          author
          (Query.commentDeck cdata)
          (Query.commentSlide cdata)
          now
  logI $ "insert comment with id: " <> show key
  json $
    View.Comment
      (Query.commentMarkdown cdata)
      (compileMarkdown (Query.commentMarkdown cdata))
      now
      (Just key)

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
          if isJust author && commentAuthor comment == author || isJust admin
            then do
              logI $ "delete comment with id: " <> show (idKey ident)
              runDb $ Sqlite.delete (idKey ident)
              status noContent204
            else status forbidden403
        Nothing -> status notFound404
    Nothing -> status forbidden403

-- | Creates and returns an admin token for the already authorized request. The
-- user name is taken from the Authorization header value.
loginAdmin :: ActionT Error EngineM ()
loginAdmin = do
  sessions <- asks adminSessions
  udb <- asks userDB
  value <- fmap toStrict <$> header "Authorization"
  let username = authUser value
  let user = username >>= (flip lookup) (users udb)
  case user of
    Just username -> do
      token <- liftIO $ calcToken value
      admin <- liftIO $ makeSessionToken' sessions username
      let adminToken = token {tokenAdmin = Just admin}
      logI $ show adminToken
      json adminToken
    Nothing -> status forbidden403

logI = lift . logInfoN
