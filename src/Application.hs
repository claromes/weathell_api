{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
module Application where

import Yesod

import Foundation
import Handler.Humidity
import Handler.Rain
import Handler.Region
import Handler.Source
import Handler.Wind

mkYesodDispatch "App" resourcesApp