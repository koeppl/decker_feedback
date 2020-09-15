{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Uri where

import Data.Aeson.TH
import Data.Char
import Database.Persist
import Database.Persist.Sql
import Network.URI (URI, URIAuth, parseURI)
import Relude

instance PersistField URI where
  toPersistValue = PersistText . show

  fromPersistValue value =
    case value of
      PersistText text -> maybe (Left "Illegal URI") Right (parseURI $ toString text)
      _ -> Left "Illegal PersistValue for field type URI"

instance PersistFieldSql URI where
  sqlType _ = SqlString

$( deriveJSON
     defaultOptions {fieldLabelModifier = drop 3 . map toLower}
     ''URI
 )

$( deriveJSON
     defaultOptions {fieldLabelModifier = drop 3 . map toLower}
     ''URIAuth
 )
