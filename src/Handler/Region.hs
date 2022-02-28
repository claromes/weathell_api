{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Handler.Region where

import Data.Aeson.Types
import Data.Maybe
import Data.Text
import Database.Persist.Postgresql
import GHC.Generics
import Network.HTTP.Types
import Yesod

import Database.Models
import Database.Utils
import Foundation

-- Get all regions
getRegionR :: Handler Value
getRegionR = do
  regions <- runDB $ selectList [] [Asc RegionId]

  sendStatusJSON ok200 $ object ["result" .= regions]

-- Get region by id
getRegionByIdR :: RegionId -> Handler Value
getRegionByIdR regionId = do
  region <- runDB $ get404 regionId

  sendStatusJSON ok200 $ object ["result" .= region]

-- Insert a new region
postRegionR :: Handler Value
postRegionR = do
  region <- requireCheckJsonBody :: Handler Region

  regionId <- runDB $ insert region

  sendStatusJSON created201
    $ object ["result" .= fromSqlKey regionId]

-- Update part of an existent region
data RegionPatch = RegionPatch
    { regionMaybeCity     :: Maybe Text
    , regionMaybeState    :: Maybe Text
    , regionMaybeCountry  :: Maybe Text
    } deriving (Generic, FromJSON)

patchRegionByIdR :: RegionId -> Handler Value
patchRegionByIdR regionId = do
  RegionPatch {..} <- requireCheckJsonBody :: Handler RegionPatch

  patchData <- return [
    column =. fromJust maybeValue
    | (column, maybeValue) <- [
        (RegionCity, regionMaybeCity),
        (RegionState, regionMaybeState),
        (RegionCountry, regionMaybeCountry)
      ]
      , isJust maybeValue
   ]

  runDB $ update regionId patchData

  sendStatusJSON noContent204 emptyObject

-- Delete a region
deleteRegionByIdR :: RegionId -> Handler Value
deleteRegionByIdR regionId = do
  runDB $ delete regionId

  sendStatusJSON noContent204 emptyObject

-- Search regions by city name
getSearchRegionByCityR :: Text -> Handler Value
getSearchRegionByCityR search = do
  regionEntities <- runDB $ selectList [RegionCity %=. search] []

  sendStatusJSON ok200 $ object [ "result" .= regionEntities ]

-- Search a region with conditions
getSearchRegionsR :: RegionId -> Handler Value
getSearchRegionsR regionId = do
  region    <- runDB $ get regionId
  source    <- runDB $ selectList [SourceRegionId ==. regionId] []
  wind      <- runDB $ selectList [WindRegionId ==. regionId] []
  rain      <- runDB $ selectList [RainRegionId ==. regionId] []
  humidity  <- runDB $ selectList [HumidityRegionId ==. regionId] []

  sendStatusJSON ok200 $
    object [
      "result" .= object [
          "region" .= region,
          "source" .= source,
          "wind" .= wind,
          "rain" .= rain,
          "humidity" .= humidity
      ]
    ]