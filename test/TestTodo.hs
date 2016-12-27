{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, Rank2Types #-}

module TestTodo where

import           Control.Lens hiding ((.=))
import           Control.Monad (mzero)
import           Data.Aeson (ToJSON(..), FromJSON(..), Value(..), object, (.=), (.:))
import           Data.List (find)
import           Data.Maybe (catMaybes, isJust)
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

mkUpdateTodoUrl :: Todo -> String
mkUpdateTodoUrl (Todo (Just i) _ _ _) = TU.mkUrl ("/api/todo/" ++ show i)
mkUpdateTodoUrl (Todo Nothing _ _ _)  = error "must call with an id"

saveTodo :: Options -> Todo -> IO Todo
saveTodo opts todo = do
  r <- asJSON =<< postWith opts (mkUpdateTodoUrl todo) (toJSON todo)
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
  let timestamp = "2016-11-11 00:00:00"
  let todo1Text = "todo 1 - updated" :: T.Text
  let todo1Id   = 1 :: Integer
  let todo1 = Todo (Just todo1Id) (read timestamp) todo1Text True
  todo' <- saveTodo opts todo1
  Just todo1Id @=? todoId todo'
  todo1Text    @=? todoText todo'
  True         @=? todoCompleted todo'

findTodo :: Todo -> [Todo] -> Maybe Todo
findTodo t =
  find (\c -> todoId c == todoId t)

todoIdExists :: Todo -> [Todo] -> Bool
todoIdExists t = isJust . findTodo t

testUserAccess :: Options -> Assertion
testUserAccess optsUser1 = do
  optsUser2 <- TU.loginUser TU.login2 TU.passwd2
  testListTodosEmpty optsUser2
  -- User1 todo list shouldn't be empty at this point (testUserAccess is
  -- called after tests that add them)
  todos <- listTodos optsUser1
  assertBool "user1 todo list not empty" (not . null $ todos)
  let user1Todo1 = todos !! 0
  assertBool "todo found" (user1Todo1 `todoIdExists` todos)
  let newTodoParams = Todo Nothing (read "2016-11-11 01:01:01") "new todo" False
  newTodo <- addTodo optsUser1 newTodoParams
  todos1User1 <- listTodos optsUser1
  todos1User2 <- listTodos optsUser2
  assertBool "todo found" (newTodo `todoIdExists` todos1User1)
  assertBool "todo not found" (not (newTodo `todoIdExists` todos1User2))
  newTodo2 <- addTodo optsUser2 newTodoParams
  todos2User1 <- listTodos optsUser1
  todos2User2 <- listTodos optsUser2
  assertBool "todo found" (newTodo2 `todoIdExists` todos2User2)
  assertBool "todo not found" (not (newTodo2 `todoIdExists` todos2User1))

testUserAccessInvalidReq :: Options -> Assertion
testUserAccessInvalidReq optsUser1 = do
  optsUser2 <- TU.loginUser TU.login2 TU.passwd2
  let newTodoParams = Todo Nothing (read "2016-11-11 01:01:01") "new todo 2" False
  newTodo <- addTodo optsUser1 newTodoParams
  -- The server shouldn't allow us to save user1's todo with user2's account
  TU.expectHttpError 403 (saveTodo optsUser2 newTodo)
  t' <- saveTodo optsUser1 newTodo
  todoText newTodoParams @=? todoText t'
