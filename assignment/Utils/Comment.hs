{-|
  Module: Utils.Comment
  Description: Module containing the definition of a JSON object representing a Comment (as seen in the HackerNews API).
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

module Utils.Comment where

{- "GHC" Library Imports -}
import GHC.Generics

{- "Data" Library Imports -}
import Data.Aeson

{- "Control" Library Imports -}
import Control.Monad

data Comment =
  Comment {
    id      :: Int,
    deleted :: Maybe Bool,
    type'   :: String,
    by      :: Maybe String,
    time    :: Maybe Int,
    dead    :: Maybe Bool,
    kids    :: Maybe [Int],
    parent  :: Maybe Int,
    text    :: Maybe String
  } deriving (Show, Generic)

instance FromJSON Comment 
  where
    parseJSON (Object o) =
      Comment 
        <$> o .:  "id"
        <*> o .:? "deleted"
        <*> o .:  "type"
        <*> o .:? "by"
        <*> o .:? "time"
        <*> o .:? "dead"
        <*> o .:? "kids"
        <*> o .:? "parent"
        <*> o .:? "text"
    parseJSON _ = mzero

instance ToJSON Comment
  where
    toJSON (Comment id deleted type' by time dead kids parent text) =
      object 
        [ "id"      .= id,
          "deleted" .= deleted,
          "type"    .= type',
          "by"      .= by,
          "time"    .= time,
          "dead"    .= dead,
          "kids"    .= kids,
          "parent"  .= parent,
          "text"    .= text ]