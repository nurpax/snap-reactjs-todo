{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, Rank2Types #-}

module Test where

import qualified Control.Exception as E
import           Control.Lens hiding ((.=))
import           Data.Aeson (object, (.=))
import           Data.Aeson.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.Wreq
import qualified Network.HTTP.Client as HT
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (TestList, Test)

mkUrl :: String -> String
mkUrl s = "http://localhost:8000" ++ s

-- Test user login name
login, login2 :: T.Text
login  = "test"
login2 = "test2"

-- Test user password
passwd, passwd2 :: T.Text
passwd  = "testpass"
passwd2 = "testpass2"

emptyPostParams :: [(BS.ByteString, BS.ByteString)]
emptyPostParams = []

setParam :: Show a => T.Text -> a -> Options -> Options
setParam name v = param name .~ [T.pack . show $ v]

setBearer :: T.Text -> Options -> Options
setBearer val =
  let b = T.encodeUtf8 val in
  header "Authorization" .~ [BS.concat ["Bearer ", b]]

-- Create two test users and run a list of subtests with JWT acquired from the
-- login process of user 'login'.
createUserTests :: [(String, Options -> Assertion)] -> IO Test
createUserTests tests = do
  r <- post (mkUrl "/api/login/new") $ object ["login" .= login,  "pass" .= passwd]
  _ <- post (mkUrl "/api/login/new") $ object ["login" .= login2, "pass" .= passwd2]
  -- We should get a JWT back
  let jwt = (r ^. responseBody . key "token" . _String)
  let opts = defaults & setBearer jwt
  return
    . testGroup "tests with created user"
    . map (\(name, test) -> testCase name (test opts)) $ tests

testLoggedInOK :: Options -> Assertion
testLoggedInOK opts = do
  r <- getWith opts (mkUrl "/api/user")
  Just "test" @=? (r ^? responseBody . key "login" . _String)

-- Login a user, return an Options with an Authorization header setup from
-- JWT.
loginUser :: T.Text -> T.Text -> IO Options
loginUser l p = do
  r <- post (mkUrl "/api/login") $ object ["login" .= l,  "pass" .= p]
  let jwt = (r ^. responseBody . key "token" . _String)
  return $ defaults & setBearer jwt

-- Login a user and run a list of subtests with cookies acquired from the
-- login process.
loginUserTests :: [(String, Options -> Assertion)] -> IO Test
loginUserTests tests = do
  opts <- loginUser login passwd
  return
    . testGroup "tests with logged in user"
    . map (\(name, test) -> testCase name (test opts)) $ tests

expectHttpError :: Int -> IO (Response LBS.ByteString) -> Assertion
expectHttpError errCode action =
  E.try action >>= check
  where
    check (Left (HT.StatusCodeException s _ _)) =
      assertBool "error ok" (s ^. statusCode == errCode)
    check (Left _)  = assertFailure "unexpected exception caught"
    check (Right _) = assertFailure "req should've failed"

-- GET requests against 'url' and expect to get an error back
getExpectHttpError :: Options -> String -> Int -> Assertion
getExpectHttpError opts url errCode = expectHttpError errCode (getWith opts url)

-- GET requests against 'url' and expect to get error 401 back because we're
-- not logged in
testLoggedInFail :: String -> Options -> Assertion
testLoggedInFail url opts = getExpectHttpError opts url 401

-- GET requests against 'url' and expect to get error 404 back
testUnknownAPIEndpoint :: String -> Assertion
testUnknownAPIEndpoint url = do
  expectHttpError 404 $ getWith defaults (mkUrl url)
  expectHttpError 404 $ postWith defaults (mkUrl url) $ object []
  expectHttpError 404 $ putWith defaults (mkUrl url) $ object []
