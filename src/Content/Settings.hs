{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards  #-}
module Content.Settings where

import Data.Aeson
import Database.Persist.Postgresql

data Settings = Settings
  { port            :: Int
  , databaseConfig  :: PostgresConf
  }

instance FromJSON Settings where
  parseJSON = withObject "Settigns" $ \obj -> do
    port            <- obj .: "port"
    databaseConfig  <- obj .: "database"

    return Settings {..}