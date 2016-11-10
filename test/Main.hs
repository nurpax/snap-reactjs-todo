{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, Rank2Types #-}

import           Network.Wreq
import           Test.Framework
import           Test.Framework.Providers.HUnit

import           Test
import           TestTodo

main :: IO ()
main =
  defaultMain
  [ testGroup "Require auth fail" requireAuthFail
  , buildTest $ createUserTests [ ("logged in after create user?", testLoggedInOK) ]
  , buildTest $ loginUserTests  [ ("logged in?",   testLoggedInOK)
                                , ("no todos yet", testListTodosEmpty)
                                , ("add one todo", testAddTodo)
                                , ("edit todo",    testUpdateTodo)
                                ]
--                                ]
--  , testCase "workout perms" testAccessRights
--  , testCase "change passwd" testChangePassword
  ]
  where
    requireAuthFail =
      map (\u -> testCase u (testLoggedInFail (mkUrl u) defaults)) authReqd
    -- REST entry points which require user to be logged in
    authReqd = [ "/api/todo"
               ]