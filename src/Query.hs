{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Query where

import Data.Aeson hiding (Key)
import Data.Aeson.TH
import Data.Char
import Database.Persist.Sqlite
import GHC.Generics
import qualified Model
import Network.URI
import Relude
import Uri ()

-- Admin login
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
  { commentMarkdown :: Maybe Text,
    commentToken :: Maybe Text,
    commentDeck :: Text,
    commentSlide :: Text,
    commentId :: Maybe (Key Model.Comment),
    commentLocation :: Maybe Text
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

data EntityId = EntityId
  { eidKey :: Int64,
    eidToken :: Maybe Text
  }
  deriving (Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = drop 3 . map toLower
       }
     ''EntityId
 )

data CommentId = CommentId
  { idKey :: Key Model.Comment,
    idToken :: Maybe Text
  }
  deriving (Generic, Show)

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

data Answer = Answer
  { answerComment :: Key Model.Comment,
    answerMarkdown :: Maybe Text,
    answerLink :: Maybe URI,
    answerToken :: Text
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = drop 6 . map toLower
       }
     ''Answer
 )
