{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}
module Foundation where

import Database.Persist.Sql
import Data.Text
import Yesod

import Database.Models

data App = App { connectionPool :: ConnectionPool }

mkYesodData "App" $(parseRoutesFile "config/definitions/routes")

instance Yesod App

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend

  runDB action = do
    App {..} <- getYesod
    runSqlPool action connectionPool