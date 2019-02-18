{-# LANGUAGE OverloadedStrings #-}
module Test.MarkdownParser where

import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T

import           Test.Tasty.HUnit

import           HttpTest.MarkdownParser
import           HttpTest.Spec


literal
  :: [Text]
  -> MessageToken
literal = MessageTokenLiteral . T.intercalate "\n"

literalMessage
  :: [Text]
  -> MessageSpec
literalMessage ts =
  MessageSpec { messageSpecTokens = [literal ts]
              , messageSpecExtractions = []
              }


unit_example_01_minimal :: Assertion
unit_example_01_minimal = do
  contents <- T.readFile "examples/01-minimal.md"
  parseFile "01-minimal.md" contents @?= Right [ ("Call 1", req, resp) ]
  where
    req = literalMessage [ "GET /status/200"
                         , "Accept: */*"
                         , "User-Agent: httptest"
                         ]

    resp = literalMessage [ "200 OK"
                          , "Content-Type: text/plain; charset=utf-8"
                          , ""
                          , "Hello, world!"
                          ]


unit_example_02_minimal_two_requests :: Assertion
unit_example_02_minimal_two_requests = do
  contents <- T.readFile "examples/02-minimal-two-requests.md"
  parseFile "01-minimal.md" contents @?= Right [ ("Call 1", req1, resp1)
                                               , ("Call 2", req2, resp2)
                                               ]
  where
    req1 = literalMessage [ "GET /status/200"
                          , "Accept: */*"
                          , "User-Agent: httptest"
                          ]

    resp1 = literalMessage [ "200 OK"
                           , "Content-Type: text/plain; charset=utf-8"
                           , ""
                           , "Hello, world!"
                           ]

    req2 = literalMessage [ "GET /status/404"
                          , "Accept: */*"
                          , "User-Agent: httptest"
                          ]

    resp2 = literalMessage [ "404 Not Found"
                           , "Content-Type: text/plain; charset=utf-8"
                           , ""
                           , "Nothing here!"
                           ]


unit_example_03_variables :: Assertion
unit_example_03_variables = do
  contents <- T.readFile "examples/03-variables.md"
  parseFile "03-variables.md" contents @?= Right [ ("Login", req1, resp1)
                                                 , ("Get User", req2, resp2)
                                                 ]
  where
    req1 = literalMessage [ "POST /login"
                          , "Accept: */*"
                          , "Content-Type: application/json"
                          , "User-Agent: httptest"
                          , ""
                          , "{"
                          , "    \"username\": \"sam\","
                          , "    \"password\": \"p@ssw0rd\""
                          , "}"
                          ]

    resp1 = MessageSpec { messageSpecTokens = [ literal [ "200 OK"
                                                        , "Content-Type: application/json; charset=utf-8"
                                                        , "Set-Cookie: auth="
                                                        ]
                                              , MessageTokenVariable (VariableIdentifier "authCookie")
                                              , MessageTokenLiteral "\n\n{\n    \"userId\": "
                                              , MessageTokenVariable (VariableIdentifier "userId")
                                              , MessageTokenLiteral "\n}"
                                              ]
                        , messageSpecExtractions = [ (VariableIdentifier "authCookie", Regex "([^;]+); .*")
                                                   , (VariableIdentifier "userId", Regex "\\d+")
                                                   ]
                        }

    req2 = MessageSpec { messageSpecTokens = [ MessageTokenLiteral "GET /user/"
                                             , MessageTokenVariable (VariableIdentifier "userId")
                                             , literal [ ""
                                                       , "Accept: */*"
                                                       , "User-Agent: httptest"
                                                       , "Cookie: auth="
                                                       ]
                                             , MessageTokenVariable (VariableIdentifier "authCookie")
                                             ]
                       , messageSpecExtractions = []
                       }

    resp2 = MessageSpec { messageSpecTokens = [ literal [ "200 OK"
                                                        , "Content-Type: application/json; charset=utf-8"
                                                        , ""
                                                        , "{"
                                                        , "    \"id\": "
                                                        ]
                                              , MessageTokenVariable (VariableIdentifier "userId")
                                              , literal [ ","
                                                        , "    \"username\": \"sam\","
                                                        , "    \"fullName\": \"Sam Roberton\""
                                                        , "}"
                                                        ]
                                              ]
                        , messageSpecExtractions = []
                        }
