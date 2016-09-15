{-# LANGUAGE OverloadedStrings
  , LambdaCase
  #-}
module PrepGhcJS.Types where

import Turtle
import Data.Either

data PrepConfig = PrepConfig
 { master        :: Turtle.FilePath
 , workdir       :: Turtle.FilePath
 , checkResolver :: IO Text
 , tag           :: Text
 , copyIgnore    :: [Text]
 , forceVersion  :: [(Text, Text)]
 , forceFresh    :: [(Text, Text)]
 , ghc           :: Text
 , extraBoot     :: [Text]
 , extraBuild     :: [Text]
 }

defPC = PrepConfig

fromPath :: Turtle.FilePath -> Text
fromPath = either (const "") id . toText
