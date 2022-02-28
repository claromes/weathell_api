{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Handler.Source where

import Data.Aeson.Types
import Data.Maybe
import Data.Text
import Database.Persist.Postgresql
import GHC.Generics
import Network.HTTP.Types
import Yesod

import Database.Models
import Foundation

-- Get all sources
getSourceR :: Handler Value
getSourceR = do
  sources <- runDB $ selectList [] [Asc SourceId]

  sendStatusJSON ok200 $ object ["result" .= sources]

-- Get source by region id
getSourceByRegionIdR :: RegionId -> Handler Value
getSourceByRegionIdR regionId = do
  sources <- runDB $ selectList [SourceRegionId ==. regionId] []

  sendStatusJSON ok200 $ object [ "result" .= sources ]

-- Insert a new source
postSourceR :: Handler Value
postSourceR = do
  source <- requireCheckJsonBody :: Handler Source

  sourceId <- runDB $ insert source

  sendStatusJSON created201
    $ object ["result" .= fromSqlKey sourceId]

-- Update part of an existent source
data SourcePatch = SourcePatch
    { sourceMaybeWeatherperson     :: Maybe Text
    , sourceMaybeOrganization      :: Maybe Text
    } deriving (Generic, FromJSON)

patchSourceByIdR :: SourceId -> Handler Value
patchSourceByIdR sourceId = do
  SourcePatch {..} <- requireCheckJsonBody :: Handler SourcePatch

  patchData <- return [
    column =. fromJust maybeValue
    | (column, maybeValue) <- [
        (SourceWeatherperson, sourceMaybeWeatherperson),
        (SourceOrganization, sourceMaybeOrganization)
      ]
      , isJust maybeValue
   ]

  runDB $ update sourceId patchData

  sendStatusJSON noContent204 emptyObject

-- Delete a source
deleteSourceByIdR :: SourceId -> Handler Value
deleteSourceByIdR sourceId = do
  runDB $ delete sourceId

  sendStatusJSON noContent204 emptyObject



