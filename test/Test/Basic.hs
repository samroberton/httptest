{-# LANGUAGE OverloadedStrings #-}
module Test.Basic where

import qualified Data.Text                as T
import qualified Network.HTTP.Client      as HTTP
import qualified Network.HTTP.Types       as HTTP
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp

import           Test.Tasty.HUnit

import           HttpTest.Parser
import           HttpTest.Runner
import           HttpTest.Spec


theRequest :: Warp.Port -> Either MkRequestError HTTP.Request
theRequest port =
  mkRequest
    HTTP.GET
    ("http://localhost:" <> T.pack (show port) <> "/status/200")
    [ ("Accept", "*/*"), ("User-Agent", "httptest") ]
    Nothing


expectedResponse :: ResponseSpec
expectedResponse =
  ResponseSpec { respSpecStatus  = HTTP.mkStatus 200 "OK"
               , respSpecHeaders = [ ResponseSpecComponent [(ResponseSpecLiteral "Content-Type: text/plain; charset=utf-8")] ]
               , respSpecBody    = Just "Hello, world!"
               }


unit_basic :: Assertion
unit_basic =
  Warp.testWithApplication (pure app) $ \port ->
    case theRequest port of
      Left e ->
        assertFailure $ show e
      Right request -> do
        response <- performRequest request
        let result = matchResponse expectedResponse response
        result @?= Right ()



app :: Wai.Application
app req respond =
  respond $ Wai.responseLBS HTTP.status200 [ ("Content-Type", "text/plain; charset=utf-8") ] "Hello, world!"
