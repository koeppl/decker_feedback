{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Docs where

import Data.Proxy

import Model

import Network.HTTP.Types
import Network.Wai

import Relude

import Servant
import Servant.API
import Servant.Docs
import Servant.Docs
import Servant.Server

import Token

import qualified View

import Web.FormUrlEncoded ( FromForm(..), ToForm(..) )

instance ToCapture (Capture "deck" Text) where
  toCapture _ = DocCapture "deck" "String id of the deck"

instance ToCapture (Capture "slide" Text) where
  toCapture _ = DocCapture "slide" "String id of the slide inside a deck"

instance ToCapture (Capture "author" Text) where
  toCapture _ = DocCapture "author" "Identifying token for a person"

instance ToCapture (Capture "token" Text) where
  toCapture _ = DocCapture "token" "Identifying token for a person"

instance ToSample Person where
  toSamples _ = singleSample (Person "decafbad")

instance ToSample Comment where
  toSamples _ = noSamples

instance ToSample View.Comment where
  toSamples _ = noSamples

instance ToSample (Key Comment) where
  toSamples _ = noSamples

instance ToSample Text where
  toSamples _ = noSamples

instance ToSample View.CommentData where
  toSamples _
    = singleSample (View.CommentData "This is not the most sensible comment:")

instance ToSample Token where
  toSamples _ = singleSample (Token "ajh4ffdgx" Nothing)

instance ToSample () where
  toSamples _ = noSamples

instance ToCapture (Capture "id" (Key Comment)) where
  toCapture _ = DocCapture "id" "Integer id a comment"

instance ToCapture (Capture "id" (Key View.Comment)) where
  toCapture _ = DocCapture "id" "Integer id a view comment"

instance ToCapture (Capture "id" (Key Person)) where
  toCapture _ = DocCapture "id" "Integer id a person"

instance MimeRender PlainText () where
  mimeRender _ _ = ""

intro
  = DocIntro
    "Welcome"
    [ "This is the decker engine webservice's API.", "Enjoy!" ]
