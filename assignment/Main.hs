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

module Main where

{- "Streamly" Library Imports -}
import Streamly
import Streamly.Prelude as SP hiding ((.:), toList, concatMap, filter, null)

{- "Data" Library Imports -}
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BSL (ByteString)
import Data.Aeson
import Data.Maybe
import Data.IORef
import Data.HashMap.Lazy hiding (filter, null)
import Data.List hiding (insert)
import Data.Either.Utils (fromRight)
import Data.Either

{- "GHC" Library Imports -}
import GHC.Generics

{- "Network" Library Imports -}
import Network.HTTP.Conduit

{- "Control" Library Imports -}
import Control.Error.Util hiding (isRight)
--import qualified Control.Monad as CM
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Exception
import Control.Concurrent.Async hiding (async)
import Control.Concurrent.MSem

{- Internal Imports -}
import qualified Utils.Story as US
import qualified Utils.Comment as UC
import Network.HnApiEmissary

main :: IO ()
main = do
  putStrLn "Commencing test..."
  let itemUrlRoot = "https://hacker-news.firebaseio.com/v0/item/"
  storyIds <- simpleHttp "https://hacker-news.firebaseio.com/v0/topstories.json?print=pretty"
  userNoOfCommentsMapIORef <- newIORef (empty :: HashMap String Int)
  SP.drain $ 
    hnConcurrentBFS itemUrlRoot storyIds userNoOfCommentsMapIORef 30 10000
  userNoOfCommentsMap <- liftIO $ readIORef userNoOfCommentsMapIORef
  putStrLn $ "The top 10 commentors, along with their no. of comments are: " <> 
    (show $ 
      Prelude.take 10 $ 
        sortBy 
          (\(_, x) (_, y) -> compare y x) $
          toList userNoOfCommentsMap)



  
