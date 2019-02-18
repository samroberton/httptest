{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Either.Validation (Validation (..))
import qualified Data.Map               as M

import qualified Network.HTTP.Client    as HTTP
import qualified Network.HTTP.Types     as HTTP

import           HttpTest.Runner
import           HttpTest.Spec


theRequest :: Validation [MkRequestError] HTTP.Request
theRequest =
  mkRequest
    HTTP.GET
    "http://httpbin.org/status/200"
    [ ("Accept", "*/*"), ("User-Agent", "httptest") ]
    Nothing


expectedResponse :: ResponseSpec
expectedResponse =
  ResponseSpec [ ResponseSpecLiteral "200 OK\nContent-Type: text/html; charset=utf-8" ]



main :: IO ()
main =
  case theRequest of
    Failure e ->
      print e
    Success request -> do
      response <- performRequest request
      let result = matchResponse expectedResponse (Environment M.empty) response
      print result
