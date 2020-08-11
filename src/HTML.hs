{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HTML where

import Network.HTTP.Media ((//), (/:))
import Relude
import Servant
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToMarkup a => MimeRender HTML a where
  mimeRender _ = encodeUtf8 . renderHtml . toHtml

instance MimeRender HTML Text.Blaze.Html.Html where
  mimeRender _ = encodeUtf8 . renderHtml
