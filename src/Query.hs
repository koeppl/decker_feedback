{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Query where

import Data.Aeson.TH
import Data.Char
import Database.Persist.Sqlite
import qualified Model
import Relude
import Data.Time

data Credentials = Credentials
  { credLogin :: Text,
    credPassword :: Text,
    credDeck :: Text
  }
  deriving (Show)

$( deriveJSON
     defaultOptions {fieldLabelModifier = drop 4 . map toLower}
     ''Credentials
 )

data CommentData = CommentData
  { commentMarkdown :: Text,
    commentToken :: Maybe Text,
    commentDeck :: Text,
    commentSlide :: Text,
    commentId :: Maybe (Key Model.Comment),
    commentAnswered :: Maybe UTCTime
  }
  deriving (Show)

$( deriveJSON
     defaultOptions {fieldLabelModifier = drop 7 . map toLower}
     ''CommentData
 )

data Select = Select
  { selectDeck :: Text,
    selectSlide :: Maybe Text,
    selectToken :: Maybe Text
  }
  deriving (Show)

$( deriveJSON
     defaultOptions {fieldLabelModifier = drop 6 . map toLower}
     ''Select
 )

data CommentId = CommentId
  { idKey :: Key Model.Comment,
    idToken :: Maybe Text
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = drop 2 . map toLower
       }
     ''CommentId
 )

data Vote = Vote
  { voteComment :: Key Model.Comment,
    voteVoter :: Text
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = drop 4 . map toLower
       }
     ''Vote
 )
