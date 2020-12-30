{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module View where

import Data.Aeson.TH
import Data.Char
import Data.Time.Clock
import qualified Model
import Network.URI
import Relude
import Uri ()

data Answer = Answer
  { answerId :: Model.Key Model.Answer,
    answerMarkdown :: Maybe Text,
    answerHtml :: Maybe Text,
    answerLink :: Maybe URI,
    answerCreated :: UTCTime
  }
  deriving (Show)

$( deriveJSON
     defaultOptions {fieldLabelModifier = drop 6 . map toLower}
     ''Answer
 )

data Comment = Comment
  { commentId :: Model.Key Model.Comment,
    commentAuthor :: Maybe Text,
    commentMarkdown :: Text,
    commentHtml :: Text,
    commentCreated :: UTCTime,
    commentSlide :: Text,
    commentVotes :: Int,
    commentDidVote :: Bool,
    commentAnswer :: [Answer]
  }
  deriving (Show)

$( deriveJSON
     defaultOptions {fieldLabelModifier = drop 7 . map toLower}
     ''Comment
 )

data CommentData = CommentData {commentDataHtml :: Text}
  deriving (Show)

$( deriveJSON
     defaultOptions {fieldLabelModifier = drop 11 . map toLower}
     ''CommentData
 )
