{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Uri where

import Data.Aeson
import Database.Persist
import Database.Persist.Sql
import Network.URI (URI, parseURI)
import Relude

instance PersistField URI where
  toPersistValue = PersistText . show

  fromPersistValue value =
    case value of
      PersistText text -> maybe (Left "Illegal URI") Right (parseURI $ toString text)
      _ -> Left "Illegal PersistValue for field type URI"

instance PersistFieldSql URI where
  sqlType _ = SqlString

instance ToJSON URI where
  toJSON uri = String $ fromString $ show uri

instance FromJSON URI where
  parseJSON (String text) = case parseURI (toString text) of
    Just uri -> return uri
    Nothing -> fail $ "Cannot parse URI from: " <> toString text
  parseJSON _ = fail "URI must be a string"

{--
$( deriveJSON
     defaultOptions {fieldLabelModifier = drop 3 . map toLower}
     ''URI
 )

$( deriveJSON
     defaultOptions {fieldLabelModifier = drop 3 . map toLower}
     ''URIAuth
 )
--}
