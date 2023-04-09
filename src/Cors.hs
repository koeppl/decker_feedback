{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cors where

import qualified Data.Text as Text
import Network.Wai
import Network.Wai.Middleware.Cors
import Relude hiding (group)

-- | Transform a comma separated string of origin URLs into a list for the
-- cors middleware.
readOrigins :: String -> Maybe ([Origin], Bool)
readOrigins line =
  case map Text.strip $ Text.splitOn "," $ toText line of
    ["*"] -> Nothing
    list -> Just (map encodeUtf8 list, True)

-- | Fill iun the course policy record
corsPolicy :: Maybe ([Origin], Bool) -> CorsResourcePolicy
corsPolicy origins =
  CorsResourcePolicy
    { corsOrigins = origins,
      corsMethods = ["GET", "HEAD", "PUT", "POST", "DELETE"],
      corsRequestHeaders = ["Authorization", "Content-Type"],
      corsExposedHeaders = Just ["Content-Type"],
      corsMaxAge = Nothing,
      corsVaryOrigin = False,
      corsRequireOrigin = False,
      corsIgnoreFailures = False
    }

-- | Creates the CORS middleware
corsWare :: IO Middleware
corsWare = do
  origins <- lookupEnv "DECKER_CORS_ORIGINS"
  let policy = corsPolicy . readOrigins <$> origins
  putStrLn $ show policy
  return $ cors $ const policy
