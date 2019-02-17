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
  -> [ResponseSpecLiteralOrVariable]
literalRespComp t = [ResponseSpecLiteral t]


literalRespComps
  :: [Text]
  -> [[ResponseSpecLiteralOrVariable]]
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
      ResponseSpec { respSpecStatus = literalRespComp "200 OK"
                   , respSpecHeaders = literalRespComps [ "Content-Type: text/plain; charset=utf-8" ]
                   , respSpecBody = literalRespComp "Hello, world!"
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
      ResponseSpec { respSpecStatus = literalRespComp "200 OK"
                   , respSpecHeaders = literalRespComps [ "Content-Type: text/plain; charset=utf-8" ]
                   , respSpecBody = literalRespComp "Hello, world!"
                   }

    req2 =
      RequestSpec { reqSpecLine1   = literalReqComp "GET /status/404"
                  , reqSpecHeaders = literalReqComps [ "Accept: */*"
                                                     , "User-Agent: httptest"
                                                     ]
                  , reqSpecBody    = []
                  }

    resp2 =
      ResponseSpec { respSpecStatus  = literalRespComp "404 Not Found"
                   , respSpecHeaders = literalRespComps [ "Content-Type: text/plain; charset=utf-8" ]
                   , respSpecBody    = literalRespComp "Nothing here!"
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
      ResponseSpec { respSpecStatus = literalRespComp "200 OK"
                   , respSpecHeaders = [ literalRespComp "Content-Type: application/json; charset=utf-8"
                                       , [ ResponseSpecLiteral "Set-Cookie: "
                                         , ResponseSpecVariableExtraction $ ExtractedVariable (VariableIdentifier "authCookie") "auth=([^;]+); .*"
                                         ]
                                       ]
                   , respSpecBody = [ ResponseSpecLiteral "{\n    \"userId\": "
                                    , ResponseSpecVariableExtraction $ ExtractedVariable (VariableIdentifier "userId") "\\d+"
                                    , ResponseSpecLiteral "\n}"
                                    ]
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
      ResponseSpec { respSpecStatus = literalRespComp "200 OK"
                   , respSpecHeaders = literalRespComps [ "Content-Type: application/json; charset=utf-8" ]
                   , respSpecBody    = [ ResponseSpecLiteral "{\n    \"id\": "
                                       , ResponseSpecVariableUsage $ VariableIdentifier "userId"
                                       , ResponseSpecLiteral ",\n    \"username\": \"sam\",\n    \"fullName\": \"Sam Roberton\"\n}"
                                       ]
                   }
