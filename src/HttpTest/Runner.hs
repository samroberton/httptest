{-# LANGUAGE NamedFieldPuns #-}
module HttpTest.Runner
    ( performRequest
    , matchResponse
    ) where

import qualified Data.ByteString.Lazy    as BL
import           Data.Maybe              (mapMaybe)
import           Data.Semigroup          ((<>))
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Types      as HTTP

import           HttpTest.Spec


performRequest :: HTTP.Request -> IO (HTTP.Response BL.ByteString)
performRequest request = do
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  HTTP.httpLbs request manager


matchResponse
  :: ResponseSpec
  -> HTTP.Response BL.ByteString
  -> Either [ResponseMatchFailure] ()
matchResponse ResponseSpec { respSpecStatus, respSpecHeaders, respSpecBody } response =
  case result of
    [] -> Right ()
    es -> Left es
  where
    result =
      matchResponseStatus respSpecStatus (HTTP.responseStatus response)
      <>
      matchResponseHeaders respSpecHeaders (HTTP.responseHeaders response)
      <>
      matchResponseBody respSpecBody (HTTP.responseBody response)


matchResponseStatus
  :: HTTP.Status
  -> HTTP.Status
  -> [ResponseMatchFailure]
matchResponseStatus expected@(HTTP.Status eCode eMsg) actual@(HTTP.Status aCode aMsg) =
  if eCode == aCode && eMsg == aMsg then
    []
  else
    [DifferentStatus expected actual]


matchResponseHeaders
  :: [HTTP.Header]
  -> [HTTP.Header]
  -> [ResponseMatchFailure]
matchResponseHeaders expected actual =
  mapMaybe f expected
  where
    f header@(n, _) =
      if header `elem` actual then
        Nothing
      else
        Just $ DifferentHeader header $ filter (\(n', _) -> n' == n) actual


matchResponseBody
  :: Maybe Text
  -> BL.ByteString
  -> [ResponseMatchFailure]

matchResponseBody Nothing actual =
  if BL.null actual then
    []
  else
    [DifferentBody Nothing (Just $ lbsToText actual)]

matchResponseBody expected@(Just et) actual =
  let
    at = lbsToText actual
  in
    if T.null at then
      [DifferentBody expected Nothing]
    else if et == at then
      []
    else
      [DifferentBody expected (Just at)]


lbsToText
  :: BL.ByteString
  -> Text
lbsToText = TL.toStrict . TLE.decodeUtf8
