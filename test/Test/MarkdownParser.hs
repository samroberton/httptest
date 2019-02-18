{-# LANGUAGE OverloadedStrings #-}
module Test.MarkdownParser where

import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T

import           Test.Tasty.HUnit

import           HttpTest.MarkdownParser
import           HttpTest.Spec


literalReqComps
  :: [Text]
  -> [RequestSpecLiteralOrVariable]
literalReqComps = (:[]) . RequestSpecLiteral . T.intercalate "\n"


literalRespComps
  :: [Text]
  -> [ResponseSpecLiteralOrVariable]
literalRespComps = (:[]) . ResponseSpecLiteral . T.intercalate "\n"


unit_example_01_minimal :: Assertion
unit_example_01_minimal = do
  contents <- T.readFile "examples/01-minimal.md"
  parseFile "01-minimal.md" contents @?= Right [ ("Call 1", req, resp) ]
  where
    req = RequestSpec ( literalReqComps [ "GET /status/200"
                                        , "Accept: */*"
                                        , "User-Agent: httptest"
                                        ]
                      )

    resp = ResponseSpec ( literalRespComps [ "200 OK"
                                           , "Content-Type: text/plain; charset=utf-8"
                                           , ""
                                           , "Hello, world!"
                                           ]
                        )


unit_example_02_minimal_two_requests :: Assertion
unit_example_02_minimal_two_requests = do
  contents <- T.readFile "examples/02-minimal-two-requests.md"
  parseFile "01-minimal.md" contents @?= Right [ ("Call 1", req1, resp1)
                                               , ("Call 2", req2, resp2)
                                               ]
  where
    req1 = RequestSpec ( literalReqComps [ "GET /status/200"
                                         , "Accept: */*"
                                         , "User-Agent: httptest"
                                         ]
                       )

    resp1 = ResponseSpec ( literalRespComps [ "200 OK"
                                            , "Content-Type: text/plain; charset=utf-8"
                                            , ""
                                            , "Hello, world!"
                                            ]
                         )

    req2 = RequestSpec ( literalReqComps [ "GET /status/404"
                                         , "Accept: */*"
                                         , "User-Agent: httptest"
                                         ]
                       )

    resp2 = ResponseSpec ( literalRespComps [ "404 Not Found"
                                            , "Content-Type: text/plain; charset=utf-8"
                                            , ""
                                            , "Nothing here!"
                                            ]
                         )


unit_example_03_variables :: Assertion
unit_example_03_variables = do
  contents <- T.readFile "examples/03-variables.md"
  parseFile "03-variables.md" contents @?= Right [ ("Login", req1, resp1)
                                                 , ("Get User", req2, resp2)
                                                 ]
  where
    req1 = RequestSpec ( literalReqComps [ "POST /login"
                                         , "Accept: */*"
                                         , "Content-Type: application/json"
                                         , "User-Agent: httptest"
                                         , ""
                                         , "{"
                                         , "    \"username\": \"sam\","
                                         , "    \"password\": \"p@ssw0rd\""
                                         , "}"
                                         ]
                       )

    resp1 = ResponseSpec ( literalRespComps [ "200 OK"
                                            , "Content-Type: application/json; charset=utf-8"
                                            , "Set-Cookie: "
                                            ]
                           <>
                           [ ResponseSpecVariableExtraction $ ExtractedVariable (VariableIdentifier "authCookie") "auth=([^;]+); .*"
                           , ResponseSpecLiteral "\n\n{\n    \"userId\": "
                           , ResponseSpecVariableExtraction $ ExtractedVariable (VariableIdentifier "userId") "\\d+"
                           , ResponseSpecLiteral "\n}"
                           ]
                         )

    req2 = RequestSpec ( [ RequestSpecLiteral "GET /user/"
                         , RequestSpecVariable (VariableIdentifier "userId")
                         ]
                         <>
                         literalReqComps [ ""
                                         , "Accept: */*"
                                         , "User-Agent: httptest"
                                         , "Cookie: "
                                         ]
                         <>
                         [RequestSpecVariable (VariableIdentifier "authCookie")]
                       )

    resp2 = ResponseSpec ( literalRespComps [ "200 OK"
                                            , "Content-Type: application/json; charset=utf-8"
                                            , ""
                                            , "{"
                                            , "    \"id\": "
                                            ]
                           <>
                           [ResponseSpecVariableUsage $ VariableIdentifier "userId"]
                           <>
                           literalRespComps [ ","
                                            , "    \"username\": \"sam\","
                                            , "    \"fullName\": \"Sam Roberton\""
                                            , "}"
                                            ]
                         )
