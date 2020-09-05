{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module View where

import Data.Aeson.TH
import Data.Char
import Data.Time.Clock

import qualified Model as Model

import Relude

data Comment
  = Comment { commentMarkdown :: Text
            , commentHtml :: Text
            , commentCreated :: UTCTime
            , commentDelete :: Maybe (Model.Key Model.Comment)
            }
  deriving ( Show )

$( deriveJSON
  defaultOptions { fieldLabelModifier = drop 7 . map toLower }
  ''Comment)

data CommentData = CommentData { commentDataHtml :: Text }
  deriving ( Show )

$( deriveJSON
  defaultOptions { fieldLabelModifier = drop 11 . map toLower }
  ''CommentData)

