{-# LANGUAGE OverloadedStrings #-}
module Basic where

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types  as HTTP

import           HttpTest.Parser
import           HttpTest.Runner
import           HttpTest.Spec

import           Test.Tasty.HUnit


theRequest :: Either MkRequestError HTTP.Request
theRequest =
  mkRequest
    HTTP.GET
    "http://httpbin.org/status/200"
    [ ("Accept", "*/*"), ("User-Agent", "httptest") ]
    Nothing


expectedResponse :: ResponseSpec
expectedResponse =
  ResponseSpec { respSpecStatus  = HTTP.mkStatus 200 "OK"
               , respSpecHeaders = [ ("Content-Type", "text/html; charset=utf-8") ]
               , respSpecBody    = Nothing
               }


unit_basic :: Assertion
unit_basic =
  case theRequest of
    Left e ->
      print e
    Right request -> do
      response <- performRequest request
      let result = matchResponse expectedResponse response
      result @?= Right ()
