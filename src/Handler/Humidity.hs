{-# LANGUAGE OverloadedStrings  #-}
module Handler.Humidity where

import Data.Aeson.Types
import Database.Persist.Postgresql
import Network.HTTP.Types
import Yesod

import Database.Models
import Foundation

-- Get all humidity infos
getHumidityR :: Handler Value
getHumidityR = do
  humidityInfos <- runDB $ selectList [] [Asc HumidityId]

  sendStatusJSON ok200 $ object ["result" .= humidityInfos]

-- Get humidity info by region id
getHumidityByRegionIdR :: RegionId -> Handler Value
getHumidityByRegionIdR regionId = do
  humidityInfos <- runDB $ selectList [ HumidityRegionId ==. regionId] []

  sendStatusJSON ok200 $ object [ "result" .= humidityInfos ]

-- Insert a new humidity info
postHumidityR :: Handler Value
postHumidityR = do
  humidityInfo <- requireCheckJsonBody :: Handler Humidity

  humidityId <- runDB $ insert humidityInfo

  sendStatusJSON created201
    $ object ["result" .= fromSqlKey humidityId]

-- Update a entire humidity info
putHumidityByIdR :: HumidityId -> Handler Value
putHumidityByIdR humidityId = do
  newHumidity <- requireCheckJsonBody :: Handler Humidity

  runDB $ replace humidityId newHumidity

  sendStatusJSON noContent204 emptyObject

-- Delete a humidity info
deleteHumidityByIdR :: HumidityId -> Handler Value
deleteHumidityByIdR humidityId = do
  runDB $ delete humidityId

  sendStatusJSON noContent204 emptyObject