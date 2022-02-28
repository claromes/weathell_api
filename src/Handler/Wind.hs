{-# LANGUAGE OverloadedStrings  #-}
module Handler.Wind where

import Data.Aeson.Types
import Database.Persist.Postgresql
import Network.HTTP.Types
import Yesod

import Database.Models
import Foundation

-- Get all wind infos
getWindR :: Handler Value
getWindR = do
  windInfos <- runDB $ selectList [] [Asc WindId]

  sendStatusJSON ok200 $ object ["result" .= windInfos]

-- Get wind info by region id
getWindByRegionIdR :: RegionId -> Handler Value
getWindByRegionIdR regionId = do
  windInfos <- runDB $ selectList [WindRegionId ==. regionId] []

  sendStatusJSON ok200 $ object [ "result" .= windInfos ]

-- Insert a new wind info
postWindR :: Handler Value
postWindR = do
  windInfo <- requireCheckJsonBody :: Handler Wind

  windId <- runDB $ insert windInfo

  sendStatusJSON created201
    $ object ["result" .= fromSqlKey windId]

-- Update a entire wind info
putWindByIdR :: WindId -> Handler Value
putWindByIdR windId = do
  newWind <- requireCheckJsonBody :: Handler Wind

  runDB $ replace windId newWind

  sendStatusJSON noContent204 emptyObject

-- Delete a wind info
deleteWindByIdR :: WindId -> Handler Value
deleteWindByIdR windId = do
  runDB $ delete windId

  sendStatusJSON noContent204 emptyObject