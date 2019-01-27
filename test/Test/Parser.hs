{-# LANGUAGE OverloadedStrings #-}
module Test.Parser where

import qualified Data.Text.IO       as T
import qualified Network.HTTP.Types as HTTP

import           Test.Tasty.HUnit

import           HttpTest.Parser
import           HttpTest.Spec



theReqSpec :: RequestSpec
theReqSpec =
  RequestSpec { reqSpecLine1 = "GET /status/200"
              , reqSpecHeaders = [ "Accept: */*"
                                 , "User-Agent: httptest"
                                 ]
              }

theRespSpec :: ResponseSpec
theRespSpec =
  ResponseSpec { respSpecStatus = HTTP.mkStatus 200 "OK"
               , respSpecHeaders = [ ("Content-Type", "text/plain; charset=utf-8") ]
               , respSpecBody = Just "Hello, world!"
               }


unit_basic :: Assertion
unit_basic = do
  contents <- T.readFile "examples/01-minimal.md"
  parseFile "01-minimal.md" contents @?= Right ("Call 1", theReqSpec, theRespSpec)
