{-# LANGUAGE RecordWildCards  #-}
module Main where

import Control.Monad.Logger
import Data.Yaml.Config
import Database.Persist.Postgresql
import Yesod

import Application ()
import Content.Settings
import Database.Models
import Foundation

main :: IO ()
main = do
  Settings {..}         <- loadYamlSettingsArgs [] ignoreEnv

  PostgresConf {..}     <- return databaseConfig

  connectionPool        <- runStdoutLoggingT $
    createPostgresqlPool pgConnStr pgPoolSize

  runSqlPersistMPool (runMigration migrateAll) connectionPool

  putStrLn "\n\n>>> Welcome to Weathell, the weather API written in Haskell! <<<\n\n"

  warp port App {..}