{-|
  Module: Utils.Story
  Description: Module containing the definition of a JSON object representing a Story (as seen in the HackerNews API).
  Copyright: 
  License:
  Maintainer: paul.reftu@outlook.de
  Stability: experimental
  Portability:
-}


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Utils.Story where

{- "GHC" Library Imports -}
import GHC.Generics

{- "Data" Library Imports -}
import Data.Aeson

{- "Control" Library Imports -}
import Control.Monad

data Story = 
  Story {
    id          :: Int, 
    deleted     :: Maybe Bool,
    type'       :: String, 
    by          :: Maybe String, 
    time        :: Maybe Int, 
    dead        :: Maybe Bool,
    kids        :: Maybe [Int], 
    descendants :: Maybe Int, 
    score       :: Maybe Int, 
    title       :: Maybe String, 
    url         :: Maybe String 
  } deriving (Show, Generic)

instance FromJSON Story 
  where
    parseJSON (Object o) =
      Story 
        <$> o .:  "id"
        <*> o .:? "deleted"
        <*> o .:  "type"
        <*> o .:? "by"
        <*> o .:? "time"
        <*> o .:? "dead"
        <*> o .:? "kids"
        <*> o .:? "descendants"
        <*> o .:? "score"
        <*> o .:? "title"
        <*> o .:? "url"
    parseJSON _ = mzero

instance ToJSON Story
  where
    toJSON (Story id deleted type' by time dead kids descendants score title url) =
      object 
        [ "id"          .= id,
          "deleted"     .= deleted,
          "type"        .= type',
          "by"          .= by,
          "time"        .= time,
          "dead"        .= dead,
          "kids"        .= kids,
          "descendants" .= descendants,
          "score"       .= score,
          "title"       .= title,
          "url"         .= url ]