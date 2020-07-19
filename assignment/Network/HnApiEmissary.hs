{-|
  Module: Network.HpApiEmissary
  Description: Methods used to obtain information w.r.t the HackerNews domain through its API.
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

module Network.HnApiEmissary where

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
import Data.Either.Utils as DEU (fromRight)
import Data.Either

{- "GHC" Library Imports -}
import GHC.Generics

{- "Network" Library Imports -}
import Network.HTTP.Conduit

{- "Control" Library Imports -}
import Control.Error.Util hiding (isRight)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Exception
import Control.Concurrent.Async hiding (async)
import Control.Concurrent.MSem

{- Internal Imports -}
import qualified Utils.Story as US
import qualified Utils.Comment as UC

-- | Run a concurrent Breadth-First Search on the Comments Tree, with the root node being the list of the top HackerNews Stories
-- and the first level of the tree consisting of each individual story respectively. Thus, every following level will contain
-- each story's comments, the level after that - each set of subcomments and so on, until the leaf comments with no further children are reached.
hnConcurrentBFS :: forall t m
  . IsStream t
  => (MonadIO m, MonadIO (t m), MonadAsync m)
  => String                     -- | The base URL used to locate the JSON resource w.r.t a given HackerNews item (i.e, a story, comment etc).
  -> BSL.ByteString             -- | The encoded unique ids of the stories whose comments we are to analyze.
  -> IORef (HashMap String Int) -- | The IORef of the hashmap that maps each unique user name to its no. of comments found among the visited stories.
  -> Int                        -- | The number of stories we are to consider. (CAUTION: The no. of top stories retrieved by the API request is currently limited at 500 as of this time).
  -> Int                        -- | The maximum height of the tree. Statistically speaking, a number as high as 10 000 or even 100 000 should be enough to rule out any outliers (i.e, deeply nested comments).
  -> t m Int 
hnConcurrentBFS itemUrlRoot storyIds userNoOfCommentsMapIORef noOfStories treeMaxHeight =
  -- Run a recursive stream that concurrently visits each comment at each level of the tree, until the maximum height is reached.
  asyncly $ hnCommentStream itemUrlRoot userNoOfCommentsMapIORef treeMaxHeight $ 
    -- Get the stream of the ids of the comments @level 2 (note: level 0 is the root node) of the tree - i.e, the comments at the top level.
    SP.concatMapWith async
        (\storyId -> do
            storyBs <- liftIO $ catch 
              (simpleHttp $ itemUrlRoot <> (show storyId) <> ".json")
              (\(he :: HttpException) -> pure mempty)
            case (eitherDecode storyBs :: Either String US.Story) of
              Left _  -> SP.nil
              Right story -> do 
                liftIO $ 
                  putStrLn $ "Story #" <> (show storyId) <> " Title: " <> 
                    (maybe "<Unknown>" id $ US.title story) 
                case (US.kids story) of
                    Just storyCommentIds -> 
                        SP.fromList storyCommentIds
                    Nothing -> SP.nil) $
        -- Obtain the stream containing the ids of the stories we are about to examine.
        SP.fromList $
          Prelude.take noOfStories $
            either (\_ -> []) Prelude.id (eitherDecode storyIds :: Either String [Int])

{- | A recursive stream that visits each level of the tree, starting from level 2, where the comment nodes begin. (level 1 contains the parent stories)
  The stream ends upon hitting the maximum tree height. 
-}
hnCommentStream :: forall t m
    . IsStream t
    => (MonadIO m, MonadIO (t m), MonadAsync m)
    => String                      -- | The root of the URL used to retrieve the JSON object w.r.t a HackerNews item.
    -> IORef (HashMap String Int)  -- | The IORef corresponding to the (username |-> noOfComments) hashmap.
    -> Int                         -- | The no. of tree levels remaining to visit.
    -> t m Int                     -- | The upstream of the current stream.
    -> t m Int
hnCommentStream itemUrlRoot userNoOfCommentsMapIORef 0 upstream = 
    upstream
hnCommentStream itemUrlRoot userNoOfCommentsMapIORef n upstream = do
    -- Execute a recursive call to be able to visit a lower level of the tree.
    adapt $ hnCommentStream itemUrlRoot userNoOfCommentsMapIORef (n - 1) $
        -- Obtain the subcomments of the set of comments coming from the upstream.
        SP.concatMapWith async
            (\commentId -> do
                {-
                    1. Obtain the JSON resource corresponding to the given comment id.
                    2. Decode said resource.
                    3. Find out the owner of the comment and update the (username |-> noOfComments) hashmap accordingly.
                    4. Get the subcomments of the current comment.
                -}
                -- 
                commentBs <- liftIO $ catch 
                    (simpleHttp $ itemUrlRoot <> (show commentId) <> ".json")
                    (\(he :: HttpException) -> pure mempty)                     -- 1
                case (eitherDecode commentBs :: Either String UC.Comment) of    -- 2
                    Left _ -> 
                        SP.nil
                    Right comment -> do
                        case (UC.by comment) of
                            Just commentCreator -> do
                                liftIO $ atomicModifyIORef' userNoOfCommentsMapIORef $
                                    \userNoOfCommentsMap ->
                                        (,) (case (member commentCreator userNoOfCommentsMap) of
                                                True -> adjust (+1) commentCreator userNoOfCommentsMap
                                                False -> insert commentCreator 1 userNoOfCommentsMap)
                                            ()
                            Nothing -> return ()                               -- 3
                        case (UC.kids comment) of
                            Just commentSubComments -> SP.fromList commentSubComments
                            Nothing -> SP.nil) $                               -- 4
            (upstream)




{-
****************************************************************************************************************************************
****************************************************************************************************************************************
****************************************************************************************************************************************

    REMARK: The following method (hnConcurrentLevelByLevel) is an *alternative* approach to solving the problem. It is far less efficient - 
        therefore, it is to be used simply for comparison, not as the ultimate solution.

****************************************************************************************************************************************
****************************************************************************************************************************************
****************************************************************************************************************************************
-}



{- | A corecursive approach towards visiting the HackerNews comments tree w.r.t the top stories. Each level of the tree
  is processed one after another, its nodes being visited concurrently, instead of the full concurrent BFS approach in the recursive
  variant above of the algorithm.

    This version is NOT scalable for a large number of stories (~ >15-20) - thus, it is not recommended to use this over the recursive variant. 
  It just serves as a comparison.
-}
hnConcurrentLevelByLevel :: forall t m
    . IsStream t
    => (MonadIO m, MonadIO (t m), MonadAsync m)
    => IORef (HashMap String Int)   -- | The (username |-> noOfComments) hashmap IORef.
    -> String                       -- | The root URL of the HackerNews items.
    -> Int                          -- | No. of stories to consider.
    -> t m ()
hnConcurrentLevelByLevel userNoOfCommentsMapIORef itemUrlRoot noOfStories =
    -- Update our hashmap according to the incoming comments.
    (asyncly $ SP.concatMapWith async
        (\(comments, _, _) -> do
            liftIO $ mapConcurrently
                (\comment -> do
                    case (UC.by comment) of
                        Just commentCreator -> do
                            liftIO $ atomicModifyIORef' userNoOfCommentsMapIORef $
                                \userNoOfCommentsMap ->
                                    (,) (case (member commentCreator userNoOfCommentsMap) of
                                            True -> adjust (+1) commentCreator userNoOfCommentsMap
                                            False -> insert commentCreator 1 userNoOfCommentsMap)
                                        ()
                            return ()
                        Nothing -> return ())
                (comments)
            SP.fromList [])          
        -- The tree will be explored until we hit the bottom level.   
        (SP.takeWhile (\(n, s, f) -> (not $ null n) || (not $ null s)) $ SP.iterateM 
            (\(nodes :: [UC.Comment], subTreeLinks :: [String], firstTime :: Bool) -> do
                semaphore <- liftIO $ new 10
                -- If it is the first iteration, then we are on level 1 of the tree, which contains the stories we are about to process.
                if (firstTime) 
                    then do
                        {- 
                            1. Obtain the links to the top n stories.
                            2. Request the JSON resources w.r.t said stories and decode them.
                            3. Print the titles of the received stories.
                            4. Get the links to all the stories' comments.
                            5. Get the comments of the stories.
                            6. Get the links to the subcomments of the stories (i.e, the comments of the comments).
                        -}
                        linksOfStories <- linksOfStoriesM itemUrlRoot noOfStories semaphore subTreeLinks -- 1
                        stories <- storiesM semaphore linksOfStories                                     -- 2
                        printStoryTitles semaphore stories                                               -- 3
                        let linksOfCommentsOfStories = fmap (\itemId -> itemUrlRoot <> itemId <> ".json") $ concatMap 
                                (maybe [] ((<$>) show) . US.kids) 
                                (stories )                                                               -- 4
                        commentsOfStories <- commentsOfStoriesM semaphore linksOfCommentsOfStories       -- 5
                        let linksOfSubcommentsOfStories = fmap (\itemId -> itemUrlRoot <> itemId <> ".json") $ concatMap
                                (maybe [] ((<$>) show) . UC.kids)
                                (commentsOfStories)                                                      -- 6
                        return $ (commentsOfStories, linksOfSubcommentsOfStories, False)
                    -- If, on the other hand, are NOT on the first iteration, that means we find ourselves in a level >= 2, which consists of comment nodes.
                    else do
                        subTrees <- subTreesM semaphore subTreeLinks
                        let linksOfNodesInSubTrees = fmap (\itemId -> itemUrlRoot <> itemId <> ".json") $ concatMap
                                (maybe [] ((<$>) show) . UC.kids)
                                (subTrees)
                        return $ (subTrees, linksOfNodesInSubTrees, False))                            
            (return $ ([], ["https://hacker-news.firebaseio.com/v0/topstories.json?print=pretty"], True)
                :: m (([UC.Comment], [String], Bool)))))
    where
        linksOfStoriesM itemUrlRoot noOfStories semaphore subTreeLinks =
            Prelude.map (\subTreeId -> itemUrlRoot <> (show subTreeId) <> ".json") 
                <$> (Prelude.take noOfStories
                <$> (Prelude.concat 
                <$> (Prelude.map (DEU.fromRight :: Either a b -> b)
                <$> (Prelude.filter (isRight :: Either a b -> Bool) 
                <$> (liftIO $ mapConcurrently (with semaphore .
                        (\subTreeLink -> do
                            subTreeBs <- liftIO $ catch 
                                (simpleHttp subTreeLink)
                                (\(he :: HttpException) -> pure mempty)
                            let eitherSubTree = eitherDecode subTreeBs :: Either String [Int]
                            return $ eitherSubTree))
                    (subTreeLinks) ))))) 
        storiesM semaphore linksOfStories =
            (Prelude.map (DEU.fromRight)
                <$> (filter (isRight :: Either a b -> Bool) 
                <$> (liftIO $ mapConcurrently (with semaphore .
                    (\linkOfStory -> do
                        storyBs <- liftIO $ catch 
                            (simpleHttp linkOfStory)
                            (\(he :: HttpException) -> pure mempty)
                        let eitherStory = eitherDecode storyBs :: Either String US.Story
                        return $ eitherStory))
                    (linksOfStories) :: m [Either String US.Story] )))
        printStoryTitles semaphore stories =
            liftIO $ mapConcurrently (with semaphore .
                (\story -> do
                    liftIO $ 
                        putStrLn $ "Story Title: " <> (maybe "<Unknown>" id $ US.title story)
                    return ()))
                (stories) 
        commentsOfStoriesM semaphore linksOfCommentsOfStories =
            Prelude.map DEU.fromRight
                <$> (filter isRight 
                <$> (liftIO $ mapConcurrently (with semaphore .
                        (\linkOfCommentsOfStories -> do
                            commentOfStoryBs <- liftIO $ catch
                                (simpleHttp linkOfCommentsOfStories)
                                (\(he :: HttpException) -> pure mempty)
                            let eitherCommentOfStory = eitherDecode commentOfStoryBs :: Either String UC.Comment
                            return $ eitherCommentOfStory))
                    (linksOfCommentsOfStories)))
        subTreesM semaphore subTreeLinks =
            Prelude.map DEU.fromRight
                <$> (filter isRight 
                <$> (liftIO $ mapConcurrently (with semaphore .
                    (\subTreeLink -> do
                        subTreeBs <- liftIO $ catch 
                            (simpleHttp subTreeLink)
                            (\(he :: HttpException) -> pure mempty)
                        let eitherSubTree = eitherDecode subTreeBs :: Either String UC.Comment
                        return $ eitherSubTree))
                    (subTreeLinks)))
  