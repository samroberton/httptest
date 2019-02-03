{-# LANGUAGE OverloadedStrings #-}
module Test.Parser where

import qualified Data.Text.IO       as T
import qualified Network.HTTP.Types as HTTP

import           Test.Tasty.HUnit

import           HttpTest.Parser
import           HttpTest.Spec



unit_example_01_minimal :: Assertion
unit_example_01_minimal = do
  contents <- T.readFile "examples/01-minimal.md"
  parseFile "01-minimal.md" contents @?= Right [ ("Call 1", req, resp) ]
  where
    req =
      RequestSpec { reqSpecLine1   = "GET /status/200"
                  , reqSpecHeaders = [ "Accept: */*"
                                     , "User-Agent: httptest"
                                     ]
                  , reqSpecBody    = Nothing
                  }

    resp =
      ResponseSpec { respSpecStatus = HTTP.mkStatus 200 "OK"
                   , respSpecHeaders = [ ("Content-Type", "text/plain; charset=utf-8") ]
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
      RequestSpec { reqSpecLine1   = "GET /status/200"
                  , reqSpecHeaders = [ "Accept: */*"
                                     , "User-Agent: httptest"
                                     ]
                  , reqSpecBody    = Nothing
                  }

    resp1 =
      ResponseSpec { respSpecStatus = HTTP.mkStatus 200 "OK"
                   , respSpecHeaders = [ ("Content-Type", "text/plain; charset=utf-8") ]
                   , respSpecBody = Just "Hello, world!"
                   }

    req2 =
      RequestSpec { reqSpecLine1   = "GET /status/404"
                  , reqSpecHeaders = [ "Accept: */*"
                                     , "User-Agent: httptest"
                                     ]
                  , reqSpecBody    = Nothing
                  }

    resp2 =
      ResponseSpec { respSpecStatus  = HTTP.mkStatus 404 "Not Found"
                   , respSpecHeaders = [ ("Content-Type", "text/plain; charset=utf-8") ]
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
      RequestSpec { reqSpecLine1   = "POST /login"
                  , reqSpecHeaders = [ "Accept: */*"
                                     , "Content-Type: application/json"
                                     , "User-Agent: httptest"
                                     ]
                  , reqSpecBody    = Just "{\n    \"username\": \"sam\",\n    \"password\": \"p@ssw0rd\"\n}"
                  }

    resp1 =
      ResponseSpec { respSpecStatus = HTTP.mkStatus 200 "OK"
                   , respSpecHeaders = [ ("Content-Type", "application/json; charset=utf-8")
                                       , ("Set-Cookie", "${{authCookie := /auth=([^;]+); .*/}}")
                                       ]
                   , respSpecBody = Just "{\n    \"userId\": ${{userId := /\\d+/}}\n}"
                   }

    req2 =
      RequestSpec { reqSpecLine1   = "GET /user/${userId}"
                  , reqSpecHeaders = [ "Accept: */*"
                                     , "User-Agent: httptest"
                                     , "Cookie: ${authCookie}"
                                     ]
                  , reqSpecBody    = Nothing
                  }

    resp2 =
      ResponseSpec { respSpecStatus = HTTP.mkStatus 200 "OK"
                   , respSpecHeaders = [ ("Content-Type", "application/json; charset=utf-8") ]
                   , respSpecBody    = Just "{\n    \"id\": ${userId},\n    \"username\": \"sam\",\n    \"fullName\": \"Sam Roberton\"\n}"
                   }
