{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Config

import Control.Monad.Logger

import Data.Maybe
import Data.Proxy
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time ( UTCTime )
import Data.Time.Clock

import Database.Persist.Sqlite

import Docs

import Mock

import Model

import Network.HTTP.Types hiding ( Header )
import Network.Wai

import Relude hiding ( get )

import Servant
import Servant.API
import Servant.Docs
import Servant.Server

import System.FilePath

import Token

import qualified View as View

import Web.FormUrlEncoded ( FromForm(..), ToForm(..) )

type GetAllComments = "comments" :> Get '[JSON] [ Comment ]

type GetComments1
  = "comments" :> Capture "deck" Text :> Get '[JSON] [ Comment ]

type GetComments2
  = "comments" :> Capture "deck" Text
  :> Capture "slide" Text :> Get '[JSON] [ View.Comment ]

type GetComments3
  = "comments" :> Capture "deck" Text :> Capture "slide" Text
  :> Capture "author" Text :> Get '[JSON] [ View.Comment ]

type PostAnonymousComment
  = "comments" :> Capture "deck" Text
  :> Capture "slide" Text :> ReqBody '[JSON] Text :> Post '[JSON] CommentId

type PostComment
  = "comments" :> Capture "deck" Text :> Capture "slide" Text
  :> Capture "author" Text :> ReqBody '[JSON] Text :> Post '[JSON] CommentId

type DeleteComment
  = "comments" :> Capture "id" (Key Comment)
  :> Capture "token" Text :> Delete '[JSON] ()

type CommentAPI
  = GetToken
  :<|> GetAllComments :<|> GetComments1 :<|> GetComments2 :<|> GetComments3
  :<|> PostAnonymousComment :<|> PostComment :<|> DeleteComment

commentAPI :: Proxy CommentAPI
commentAPI = Proxy

commentServer :: Server CommentAPI
commentServer
  = getToken
  :<|> getAllComments
  :<|> getByDeckComments
  :<|> getBySlideComments
  :<|> getBySlideAuthorComments
  :<|> postAnonymousComment
  :<|> postComment
  :<|> deleteComment

type AuthorAPI = GetAllAuthors :<|> GetAuthor

type GetAllAuthors = "authors" :> Get '[JSON] [ Person ]

type GetAuthor = "authors" :> Capture "id" (Key Person) :> Get '[JSON] Person

authorAPI :: Proxy AuthorAPI
authorAPI = Proxy

authorServer :: Server AuthorAPI
authorServer = getAllAuthors :<|> getAuthor

type JsAPI = CommentAPI :<|> AuthorAPI

jsAPI :: Proxy JsAPI
jsAPI = Proxy

docs :: Text
docs = toText . markdown $ docsWithIntros [ intro ] deckerAPI

saveDocs :: FilePath -> IO ()
saveDocs path = Text.writeFile path API.docs

type DeckerAPI = CommentAPI :<|> AuthorAPI :<|> Raw

deckerAPI :: Proxy DeckerAPI
deckerAPI = Proxy

deckerServer :: Server DeckerAPI
deckerServer
  = commentServer :<|> authorServer :<|> serveDirectoryWebApp "static"

getAllComments :: Handler [ Comment ]
getAllComments
  = liftIO
  $ do runSqlite "db/engine.db" $ do map entityVal <$> selectList [] []

getByDeckComments :: Text -> Handler [ Comment ]
getByDeckComments id
  = liftIO
  $ runSqlite "db/engine.db"
  $ do map entityVal <$> selectList [ CommentDeck ==. id ] []

getBySlideComments :: Text -> Text -> Handler [ View.Comment ]
getBySlideComments did sid
  = liftIO
  $ runSqlite "db/engine.db"
  $ do list <- map entityVal
         <$> selectList
           [ CommentDeck ==. did, CommentSlide ==. sid ]
           [ Desc CommentCreated ]
       return $ map toView list
  where
    toView c = View.Comment (commentMarkdown c) (commentCreated c) Nothing

getByAuthorComments :: Text -> Handler [ Comment ]
getByAuthorComments token
  = liftIO
  $ runSqlite "db/engine.db"
  $ do author <- fmap entityKey <$> selectFirst [ PersonToken ==. token ] []
       map entityVal
         <$> selectList [ CommentAuthor ==. author ] [ Desc CommentCreated ]

getBySlideAuthorComments :: Text -> Text -> Text -> Handler [ View.Comment ]
getBySlideAuthorComments did sid token
  = liftIO
  $ runSqlite "db/engine.db"
  $ do author <- fmap entityKey <$> selectFirst [ PersonToken ==. token ] []
       list <- selectList
         [ CommentDeck ==. did, CommentSlide ==. sid ]
         [ Desc CommentCreated ]
       return $ map (toView author) list
  where
    toView a e
      = let c = entityVal e
        in View.Comment
             (commentMarkdown c)
             (commentCreated c)
             (if isJust a && commentAuthor c == a
                 then Just (entityKey e)
                 else Nothing)

postComment :: Text -> Text -> Text -> Text -> Handler CommentId
postComment did sid token markdown
  = liftIO
  $ do now <- getCurrentTime
       runSqlite "db/engine.db"
         $ do when (Text.null did || Text.null sid) $ fail "Fucking idiot."
              maybeKey
                <- fmap entityKey <$> selectFirst [ PersonToken ==. token ] []
              case maybeKey of
                Just key -> insert $ Comment markdown did sid maybeKey now
                Nothing -> do key <- insert $ Person token
                              insert $ Comment markdown did sid (Just key) now

postAnonymousComment :: Text -> Text -> Text -> Handler CommentId
postAnonymousComment did sid markdown
  = liftIO
  $ do now <- getCurrentTime
       runSqlite "db/engine.db"
         $ do when (Text.null did || Text.null sid) $ fail "Fucking idiot."
              insert $ Comment markdown did sid Nothing now

deleteComment :: Key Comment -> Text -> Handler ()
deleteComment key token
  = liftIO
  $ runSqlite "db/engine.db"
  $ do author <- fmap entityKey <$> selectFirst [ PersonToken ==. token ] []
       comment <- get key
       if (isJust author
           && isJust comment
           && commentAuthor (fromJust comment) == author)
          then delete key
          else return ()

getAllAuthors :: Handler [ Person ]
getAllAuthors
  = liftIO $ runSqlite "db/engine.db" $ do map entityVal <$> selectList [] []

getAuthor :: Key Person -> Handler Person
getAuthor key = liftIO $ runSqlite "db/engine.db" $ do getJust key
