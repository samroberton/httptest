{-# LANGUAGE OverloadedStrings #-}
module Test.Parser where

import           Data.Text          (Text)
import qualified Data.Text.IO       as T
import qualified Network.HTTP.Types as HTTP

import           Test.Tasty.HUnit

import           HttpTest.Parser
import           HttpTest.Spec


literalReqComp
  :: Text
  -> [RequestSpecLiteralOrVariable]
literalReqComp t = [RequestSpecLiteral t]


literalReqComps
  :: [Text]
  -> [[RequestSpecLiteralOrVariable]]
literalReqComps = fmap literalReqComp


literalRespComp
  :: Text
  -> ResponseSpecComponent
literalRespComp = ResponseSpecComponent . (:[]) . ResponseSpecLiteral


literalRespComps
  :: [Text]
  -> [ResponseSpecComponent]
literalRespComps = fmap literalRespComp


unit_example_01_minimal :: Assertion
unit_example_01_minimal = do
  contents <- T.readFile "examples/01-minimal.md"
  parseFile "01-minimal.md" contents @?= Right [ ("Call 1", req, resp) ]
  where
    req =
      RequestSpec { reqSpecLine1   = literalReqComp "GET /status/200"
                  , reqSpecHeaders = literalReqComps [ "Accept: */*"
                                                     , "User-Agent: httptest"
                                                     ]
                  , reqSpecBody    = []
                  }

    resp =
      ResponseSpec { respSpecStatus = HTTP.mkStatus 200 "OK"
                   , respSpecHeaders = literalRespComps [ "Content-Type: text/plain; charset=utf-8" ]
                   , respSpecBody = Just "Hello, world!"
                   }


unit_example_02_minimal_two_requests :: Assertion
unit_example_02_minimal_two_requests = do
  contents <- T.readFile "examples/02-minimal-two-requests.md"
  parseFile "01-minimal.md" contents @?= Right [ ("Call 1", req1, resp1)
                                               , ("Call 2", req2, resp2)
                                               ]
  where
    req1 =
      RequestSpec { reqSpecLine1   = literalReqComp "GET /status/200"
                  , reqSpecHeaders = literalReqComps [ "Accept: */*"
                                                     , "User-Agent: httptest"
                                                     ]
                  , reqSpecBody    = []
                  }

    resp1 =
      ResponseSpec { respSpecStatus = HTTP.mkStatus 200 "OK"
                   , respSpecHeaders = literalRespComps [ "Content-Type: text/plain; charset=utf-8" ]
                   , respSpecBody = Just "Hello, world!"
                   }

    req2 =
      RequestSpec { reqSpecLine1   = literalReqComp "GET /status/404"
                  , reqSpecHeaders = literalReqComps [ "Accept: */*"
                                                     , "User-Agent: httptest"
                                                     ]
                  , reqSpecBody    = []
                  }

    resp2 =
      ResponseSpec { respSpecStatus  = HTTP.mkStatus 404 "Not Found"
                   , respSpecHeaders = literalRespComps [ "Content-Type: text/plain; charset=utf-8" ]
                   , respSpecBody    = Just "Nothing here!"
                   }


unit_example_03_variables :: Assertion
unit_example_03_variables = do
  contents <- T.readFile "examples/03-variables.md"
  parseFile "03-variables.md" contents @?= Right [ ("Login", req1, resp1)
                                                 , ("Get User", req2, resp2)
                                                 ]
  where
    req1 =
      RequestSpec { reqSpecLine1   = literalReqComp "POST /login"
                  , reqSpecHeaders = literalReqComps [ "Accept: */*"
                                                     , "Content-Type: application/json"
                                                     , "User-Agent: httptest"
                                                     ]
                  , reqSpecBody    = literalReqComp "{\n    \"username\": \"sam\",\n    \"password\": \"p@ssw0rd\"\n}"
                  }

    resp1 =
      ResponseSpec { respSpecStatus = HTTP.mkStatus 200 "OK"
                   , respSpecHeaders = [ ResponseSpecComponent [ResponseSpecLiteral "Content-Type: application/json; charset=utf-8"]
                                         -- FIXME response variable extraction
                                       , ResponseSpecComponent [ ResponseSpecLiteral "Set-Cookie: "
                                                               , ResponseSpecVariableExtraction $ ExtractedVariable (VariableIdentifier "authCookie") "auth=([^;]+); .*"
                                                               ]
                                       ]
                   -- FIXME response variable extraction in body
                   , respSpecBody = Just "{\n    \"userId\": ${{userId := /\\d+/}}\n}"
                   }

    req2 =
      RequestSpec { reqSpecLine1   = [ RequestSpecLiteral "GET /user/"
                                     , RequestSpecVariable (VariableIdentifier "userId")
                                     ]
                  , reqSpecHeaders = [ literalReqComp "Accept: */*"
                                     , literalReqComp "User-Agent: httptest"
                                     , [ RequestSpecLiteral "Cookie: "
                                       , RequestSpecVariable (VariableIdentifier "authCookie")
                                       ]
                                     ]
                  , reqSpecBody    = []
                  }

    resp2 =
      ResponseSpec { respSpecStatus = HTTP.mkStatus 200 "OK"
                   , respSpecHeaders = literalRespComps [ "Content-Type: application/json; charset=utf-8" ]
                   -- FIXME response variable usage (in body)
                   , respSpecBody    = Just "{\n    \"id\": ${userId},\n    \"username\": \"sam\",\n    \"fullName\": \"Sam Roberton\"\n}"
                   }
