{-# LANGUAGE OverloadedStrings #-}
module Test.Basic where

import           Data.Either.Validation   (Validation (..))
import qualified Data.Text                as T
import qualified Network.HTTP.Client      as HTTP
import qualified Network.HTTP.Types       as HTTP
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp

import           Test.Tasty.HUnit

import           HttpTest.Parser
import           HttpTest.Runner
import           HttpTest.Spec


theRequest :: Warp.Port -> Validation [MkRequestError] HTTP.Request
theRequest port =
  mkRequest
    HTTP.GET
    ("http://localhost:" <> T.pack (show port) <> "/status/200")
    [ ("Accept", "*/*"), ("User-Agent", "httptest") ]
    Nothing


expectedResponse :: ResponseSpec
expectedResponse =
  ResponseSpec { respSpecStatus  = [ResponseSpecLiteral "200 OK"]
               , respSpecHeaders = [[ResponseSpecLiteral "Content-Type: text/plain; charset=utf-8"]]
               , respSpecBody    = [ResponseSpecLiteral "Hello, world!"]
               }


unit_basic :: Assertion
unit_basic =
  Warp.testWithApplication (pure app) $ \port ->
    case theRequest port of
      Failure e ->
        assertFailure $ show e
      Success request -> do
        response <- performRequest request
        let result = matchResponse expectedResponse response
        result @?= Success ()



app :: Wai.Application
app req respond =
  respond $ Wai.responseLBS HTTP.status200 [ ("Content-Type", "text/plain; charset=utf-8") ] "Hello, world!"
