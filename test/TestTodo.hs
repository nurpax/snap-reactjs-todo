{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, Rank2Types #-}

module TestTodo where

import           Control.Lens hiding ((.=))
import           Control.Monad (mzero)
import           Data.Aeson (ToJSON(..), FromJSON(..), Value(..), object, (.=), (.:))
import           Data.Aeson.Lens
import           Data.List (find)
import           Data.Maybe (catMaybes, fromJust, isJust)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Network.Wreq
import           Test.HUnit hiding (TestList, Test)

import qualified Test as TU

data Todo = Todo {
    todoId        :: Maybe Integer
  , todoSavedOn   :: UTCTime
  , todoText      :: T.Text
  , todoCompleted :: Bool
  } deriving (Show, Eq)

instance ToJSON Todo where
  toJSON c =
    object . catMaybes $ [ fmap ("id" .=) $ todoId c
                         , Just $ "savedOn"   .= todoSavedOn c
                         , Just $ "completed" .= todoCompleted c
                         , Just $ "text"      .= todoText c
                         ]

instance FromJSON Todo where
  parseJSON (Object v) = Todo <$> (v .: "id") <*> (v .: "savedOn") <*>  v .: "text" <*> v .: "completed"
  parseJSON _          = mzero


listTodos :: Options -> IO [Todo]
listTodos opts = do
  r <- asJSON =<< getWith opts (TU.mkUrl "/api/todo")
  return (r ^. responseBody)

testListTodosEmpty :: Options -> Assertion
testListTodosEmpty opts = do
  todos <- listTodos opts
  [] @=? todos

todo1 :: Todo
todo1 = Todo Nothing (read "2016-11-02 00:00:00") "todo 1" False

addTodo :: Options -> Todo -> IO Todo
addTodo opts todo = do
  r <- asJSON =<< postWith opts (TU.mkUrl "/api/todo") (toJSON todo)
  return (r ^. responseBody)

testAddTodo :: Options -> Assertion
testAddTodo opts = do
  r <- addTodo opts todo1
  Just 1              @=? todoId r
  todoText todo1      @=? todoText r
  todoCompleted todo1 @=? todoCompleted r
  todos <- listTodos opts
  let todo = todos !! 0
  (Just 1, todoText todo1, False) @=? (todoId todo, todoText todo, todoCompleted todo)

testUpdateTodo :: Options -> Assertion
testUpdateTodo opts = do
  let timestamp = "2016-11-11T00:00:00+00" :: T.Text
  let todo1Text = "todo 1 - updated" :: T.Text
  let todo1Id   = 1 :: Integer
  let params = object [ "id"        .= todo1Id
                      , "completed" .= True
                      , "savedOn"   .= timestamp
                      , "text"      .= todo1Text
                      ]
  r <- postWith opts (TU.mkUrl "/api/todo") $ params
  Just todo1Id   @=? r ^? responseBody . key "id" . _Integer
  Just todo1Text @=? r ^? responseBody . key "text" . _String
  Just True      @=? r ^? responseBody . key "completed" . _Bool
  assertBool "savedOn set" (isJust $ r ^? responseBody . key "savedOn" . _String)

findTodo :: Todo -> [Todo] -> Maybe Todo
findTodo t =
  find (\c -> todoId c == todoId t)

todoIdElem :: Todo -> [Todo] -> Bool
todoIdElem t = isJust . findTodo t

testUserAccess :: Options -> Assertion
testUserAccess optsUser1 = do
  optsUser2 <- TU.loginUser TU.login2 TU.passwd2
  testListTodosEmpty optsUser2
  -- User1 todo list shouldn't be empty at this point (testUserAccess is
  -- called after tests that add them)
  todos <- listTodos optsUser1
  assertBool "user1 todo list not empty" (not . null $ todos)
  let user1Todo1 = todos !! 0
  assertBool "todo found" (user1Todo1 `todoIdElem` todos)
  let newTodoParams = Todo Nothing (read "2016-11-11 01:01:01") "new todo" False
  newTodo <- addTodo optsUser1 newTodoParams
  todos1User1 <- listTodos optsUser1
  todos1User2 <- listTodos optsUser2
  assertBool "todo found" (newTodo `todoIdElem` todos1User1)
  assertBool "todo not found" (not (newTodo `todoIdElem` todos1User2))
  newTodo2 <- addTodo optsUser2 newTodoParams
  todos2User1 <- listTodos optsUser1
  todos2User2 <- listTodos optsUser2
  assertBool "todo found" (newTodo2 `todoIdElem` todos2User2)
  assertBool "todo not found" (not (newTodo2 `todoIdElem` todos2User1))
  -- TODO add a test that will try to update a todo owned by user1 using user2
  return ()
