{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, Rank2Types #-}

module TestTodo where

import           Control.Lens hiding ((.=))
import           Data.Aeson (object, (.=))
import           Data.Aeson.Lens
import           Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import           Network.Wreq
import           Test.HUnit hiding (TestList, Test)

import           Test

type Todo = (Integer, T.Text, Bool)

listTodos :: Options -> IO [Todo]
listTodos opts = do
  r <- getWith opts (mkUrl "/api/todo")
  return $ r ^.. responseBody . values . to todo
  where
    todo v = (fromJust $ v ^? key "id" . _Integer,
              v ^. key "text" . _String,
              fromJust $ v ^? key "completed" . _Bool)

testListTodosEmpty :: Options -> Assertion
testListTodosEmpty opts = do
  todos <- listTodos opts
  [] @=? todos

testAddTodo :: Options -> Assertion
testAddTodo opts = do
  let todo1Text = "todo 1" :: T.Text
  r <- postWith opts (mkUrl "/api/todo") $ object [ "completed" .= False, "text" .= todo1Text]
  Just 1         @=? r ^? responseBody . key "id" . _Integer
  Just todo1Text @=? r ^? responseBody . key "text" . _String
  Just False     @=? r ^? responseBody . key "completed" . _Bool
  assertBool "savedOn set" (isJust $ r ^? responseBody . key "savedOn" . _String)
  todos <- listTodos opts
  [(1, todo1Text, False)] @=? todos

testUpdateTodo :: Options -> Assertion
testUpdateTodo opts = do
  let timestamp = "2016-11-11T00:00:00+00" :: T.Text
  let todo1Text = "todo 1 - updated" :: T.Text
  let todo1Id   = 1 :: Integer
  r <- postWith opts (mkUrl "/api/todo") $ object [ "id"        .= todo1Id
                                                  , "completed" .= True
                                                  , "savedOn"   .= timestamp
                                                  , "text"      .= todo1Text]
  Just todo1Id   @=? r ^? responseBody . key "id" . _Integer
  Just todo1Text @=? r ^? responseBody . key "text" . _String
  Just True      @=? r ^? responseBody . key "completed" . _Bool
  assertBool "savedOn set" (isJust $ r ^? responseBody . key "savedOn" . _String)
