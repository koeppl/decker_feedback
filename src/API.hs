{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import Config
import Docs
import Mock
import Model

import Control.Monad.Logger
import Data.Maybe
import Data.Proxy
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Time.Clock
import Database.Persist.Sqlite
import Network.HTTP.Types
import Network.Wai
import Relude hiding (get)
import Servant
import Servant.API
import Servant.Docs
import Servant.Server
import Web.FormUrlEncoded (FromForm(..), ToForm(..))

type GetAllComments = "comments" :> Get '[ JSON] [Comment]

type GetComments1 = "comments" :> Capture "deck" Text :> Get '[ JSON] [Comment]

type GetComments2
   = "comments" :> Capture "deck" Text :> Capture "slide" Text :> Get '[ JSON] [( Key Comment
                                                                                , Comment)]

type GetComments3
   = "comments" :> Capture "deck" Text :> Capture "slide" Text :> Capture "author" Text :> Get '[ JSON] [Comment]

type PostComment
   = "comments" :> Capture "deck" Text :> Capture "slide" Text :> Capture "author" Text :> ReqBody '[ JSON] Text :> Post '[ JSON] CommentId

type DeleteComment
   = "comments" :> Capture "id" (Key Comment) :> Delete '[ JSON] ()

type CommentAPI
   = GetAllComments :<|> GetComments1 :<|> GetComments2 :<|> GetComments3 :<|> PostComment :<|> DeleteComment

commentAPI :: Proxy CommentAPI
commentAPI = Proxy

commentServer :: Server CommentAPI
commentServer =
  getAllComments :<|> getByDeckComments :<|> getBySlideComments :<|>
  getBySlideAuthorComments :<|>
  postComment :<|>
  deleteComment

type AuthorAPI = GetAllAuthors :<|> GetAuthor

type GetAllAuthors = "authors" :> Get '[ JSON] [Person]

type GetAuthor = "authors" :> Capture "id" (Key Person) :> Get '[ JSON] Person

authorAPI :: Proxy AuthorAPI
authorAPI = Proxy

authorServer :: Server AuthorAPI
authorServer = getAllAuthors :<|> getAuthor

type JsAPI = CommentAPI :<|> AuthorAPI

jsAPI :: Proxy JsAPI
jsAPI = Proxy

docsBS :: LByteString
docsBS = encodeUtf8 . toText . markdown $ docsWithIntros [intro] deckerAPI

serveDocs _ respond =
  respond $ responseLBS ok200 [("Content-Type", "text/plain")] docsBS

type DeckerAPI = CommentAPI :<|> AuthorAPI :<|> Raw :<|> Raw

deckerAPI :: Proxy DeckerAPI
deckerAPI = Proxy

deckerServer :: Server DeckerAPI
deckerServer =
  commentServer :<|> authorServer :<|> Tagged serveDocs :<|>
  serveDirectoryWebApp "public"

getAllComments :: Handler [Comment]
getAllComments =
  liftIO $ do
    putStrLn "get all comments"
    runSqlite "db/engine.db" $ do map entityVal <$> selectList [] []

getByDeckComments :: Text -> Handler [Comment]
getByDeckComments id =
  liftIO $
  runSqlite "db/engine.db" $ do
    map entityVal <$> selectList [CommentDeck ==. id] []

getBySlideComments :: Text -> Text -> Handler [(Key Comment, Comment)]
getBySlideComments did sid =
  liftIO $
  runSqlite "db/engine.db" $ do
    list <-
      selectList
        [CommentDeck ==. did, CommentSlide ==. sid]
        [Desc CommentCreated]
    return $ map (\e -> (entityKey e, entityVal e)) list

getByAuthorComments :: Text -> Handler [Comment]
getByAuthorComments token =
  liftIO $
  runSqlite "db/engine.db" $ do
    author <- fmap entityKey <$> selectFirst [PersonToken ==. token] []
    map entityVal <$>
      selectList [CommentAuthor ==. author] [Desc CommentCreated]

getBySlideAuthorComments :: Text -> Text -> Text -> Handler [Comment]
getBySlideAuthorComments did sid token =
  liftIO $
  runSqlite "db/engine.db" $ do
    author <- fmap entityKey <$> selectFirst [PersonToken ==. token] []
    map entityVal <$>
      selectList
        [CommentAuthor ==. author, CommentDeck ==. did, CommentSlide ==. sid]
        [Desc CommentCreated]

postComment :: Text -> Text -> Text -> Text -> Handler CommentId
postComment did sid token markdown =
  liftIO $ do
    now <- getCurrentTime
    runSqlite "db/engine.db" $ do
      when (Text.null did || Text.null sid) $ fail "Fucking idiot."
      if Text.null token
        then insert $ Comment markdown did sid Nothing now
        else do
          maybeKey <- fmap entityKey <$> selectFirst [PersonToken ==. token] []
          case maybeKey of
            Just key -> insert $ Comment markdown did sid maybeKey now
            Nothing -> do
              key <- insert $ Person token
              insert $ Comment markdown did sid (Just key) now

deleteComment :: Key Comment -> Handler ()
deleteComment key = liftIO $ runSqlite "db/engine.db" $ do delete key

getAllAuthors :: Handler [Person]
getAllAuthors =
  liftIO $ runSqlite "db/engine.db" $ do map entityVal <$> selectList [] []

getAuthor :: Key Person -> Handler Person
getAuthor key = liftIO $ runSqlite "db/engine.db" $ do getJust key
