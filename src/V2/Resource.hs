{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module V2.Resource where

-- import Data.Aeson
import GHC.Generics
import Network.URI
import Relude
import View

data Resource a = Resource
  { id :: Int64,
    entity :: a,
    links :: [(Text, URI)]
  }
  deriving (Generic, Show)

newtype CommentResource = CommentResource (Resource Comment)

{--
instance ToJSON a => ToJSON (Resource a)

instance ToJSON URI where
  toJSON uri = String $ fromString $ show uri

instance FromJSON URI where
  parseJSON (String text) = case parseRelativeReference (toString text) of
    Just uri -> return uri
    Nothing -> fail $ "Cannot parse URI from: " <> toString text
  parseJSON _ = fail "URI must be a string"
--}
