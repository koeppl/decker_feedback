{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module API where

import Admin
import Commonmark
import Config
import Control.Monad.Logger
import Data.Maybe
import Data.Proxy
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.Builder as Text
import Data.Time (UTCTime)
import Data.Time.Clock
import Database.Persist.Sqlite
import Docs
import Mock
import Model
import Network.HTTP.Types hiding (Header)
import Network.Wai
import Query
import Relude hiding (get)
import Servant
import Servant.API
import Servant.Docs
import Servant.Server
import System.FilePath
import Token
import qualified View as View
import Web.FormUrlEncoded (FromForm (..), ToForm (..))

type GetComments =
  "comments" :> ReqBody '[JSON] Query.Select :> Put '[JSON] [View.Comment]

type PostComment =
  "comments" :> ReqBody '[JSON] Query.CommentData :> Post '[JSON] View.Comment

type DeleteComment =
  "comments" :> ReqBody '[JSON] Query.CommentId :> DeleteAccepted '[JSON] ()

type CommentAPI = GetToken :<|> GetComments :<|> PostComment :<|> DeleteComment

commentAPI :: Proxy CommentAPI
commentAPI = Proxy

commentServer :: Server CommentAPI
commentServer = getToken :<|> getComments :<|> postComment :<|> deleteComment

type AuthorAPI = GetAllAuthors :<|> GetAuthor

type GetAllAuthors = "authors" :> Get '[JSON] [Person]

type GetAuthor = "authors" :> Capture "id" (Key Person) :> Get '[JSON] Person

authorAPI :: Proxy AuthorAPI
authorAPI = Proxy

authorServer :: Server AuthorAPI
authorServer = getAllAuthors :<|> getAuthor

type JsAPI = CommentAPI :<|> AuthorAPI

jsAPI :: Proxy JsAPI
jsAPI = Proxy

-- docs :: Text
-- docs = toText . markdown $ docsWithIntros [ intro ] deckerAPI
-- saveDocs :: FilePath -> IO ()
-- saveDocs path = Text.writeFile path API.docs
type DeckerAPI = AuthAPI :<|> CommentAPI :<|> AuthorAPI :<|> Raw

deckerAPI :: Proxy DeckerAPI
deckerAPI = Proxy


deckerServer :: Server DeckerAPI
deckerServer =
  authServer :<|> commentServer :<|> authorServer :<|> serveDirectoryWebApp "static"

-- | Retrieves the selected list of comments from the database. If the slide id
-- fragment is specified, only comments for that slide are returned, otherwise
-- all comments for the deck are returned. If the author token is specified,
-- the comment id is set to allow deletion.
getComments :: Query.Select -> Handler [View.Comment]
getComments selector =
  liftIO $ do
    print selector
    runSqlite "db/engine.db" $ do
      author <- case selectToken selector of
        Just token ->
          fmap entityKey
            <$> selectFirst [PersonToken ==. token] []
        Nothing -> return Nothing
      list <- case selectSlide selector of
        Just slideId ->
          selectList
            [ CommentDeck ==. selectDeck selector,
              CommentSlide ==. slideId
            ]
            [Desc CommentCreated]
        Nothing ->
          selectList
            [CommentDeck ==. selectDeck selector]
            [Desc CommentCreated]
      return $ map (toView author) list
  where
    toView a e =
      let c = entityVal e
       in View.Comment
            (Model.commentMarkdown c)
            (Model.commentHtml c)
            (commentCreated c)
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

postComment :: Query.CommentData -> Handler View.Comment
postComment cdata =
  liftIO $ do
    print cdata
    now <- getCurrentTime
    runSqlite "db/engine.db" $ do
      author <- case commentToken cdata of
        Just token -> do
          key <-
            fmap entityKey
              <$> selectFirst [PersonToken ==. token] []
          case key of
            Just key -> return $ Just key
            Nothing -> Just <$> insert (Person token)
        Nothing -> return Nothing
      key <-
        insert $
          Comment
            (Query.commentMarkdown cdata)
            (compileMarkdown (Query.commentMarkdown cdata))
            author
            (Query.commentDeck cdata)
            (Query.commentSlide cdata)
            now
      return $
        View.Comment
          (Query.commentMarkdown cdata)
          (compileMarkdown (Query.commentMarkdown cdata))
          now
          (Just key)

deleteComment :: Query.CommentId -> Handler ()
deleteComment ident =
  liftIO $ do
    print ident
    runSqlite "db/engine.db" $
      do
        author <- case idToken ident of
          Just token ->
            fmap entityKey <$> selectFirst [PersonToken ==. token] []
          Nothing ->
            return Nothing
        comment <- get (idKey ident)
        if ( isJust author
               && isJust comment
               && commentAuthor (fromJust comment) == author
           )
          then delete (idKey ident)
          else return ()

getAllAuthors :: Handler [Person]
getAllAuthors =
  liftIO $ runSqlite "db/engine.db" $ map entityVal <$> selectList [] []

getAuthor :: Key Person -> Handler Person
getAuthor key = liftIO $ runSqlite "db/engine.db" $ getJust key
