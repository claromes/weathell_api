{-# LANGUAGE OverloadedStrings  #-}
module Handler.Rain where

import Data.Aeson.Types
import Database.Persist.Postgresql
import Network.HTTP.Types
import Yesod

import Database.Models
import Foundation

-- Get all rain infos
getRainR :: Handler Value
getRainR = do
  rainInfos <- runDB $ selectList [] [Asc RainId]

  sendStatusJSON ok200 $ object ["result" .= rainInfos]

-- Get rain info by region id
getRainByRegionIdR :: RegionId -> Handler Value
getRainByRegionIdR regionId = do
  rainInfos <- runDB $ selectList [RainRegionId ==. regionId] []

  sendStatusJSON ok200 $ object [ "result" .= rainInfos ]

-- Insert a new rain info
postRainR :: Handler Value
postRainR = do
  rainInfo <- requireCheckJsonBody :: Handler Rain

  rainId <- runDB $ insert rainInfo

  sendStatusJSON created201
    $ object ["result" .= fromSqlKey rainId]

-- Update a entire rain info
putRainByIdR :: RainId -> Handler Value
putRainByIdR rainId = do
  newRain <- requireCheckJsonBody :: Handler Rain

  runDB $ replace rainId newRain

  sendStatusJSON noContent204 emptyObject

-- Delete a rain info
deleteRainByIdR :: RainId -> Handler Value
deleteRainByIdR rainId = do
  runDB $ delete rainId

  sendStatusJSON noContent204 emptyObject

-- Get your umbrella
getRainCheckR :: Handler Value
getRainCheckR = do
  rainCheck <- runDB $ selectList [RainProbability >. 50] []

  sendStatusJSON ok200 $ object [ "result" .= rainCheck ]