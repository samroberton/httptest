{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module HttpTest.Runner
    ( MkRequestError
    , matchResponse
    , mkRequest
    , performRequest
    ) where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.CaseInsensitive    as CI
import           Data.Either.Validation  (Validation (..))
import           Data.Maybe              (mapMaybe)
import           Data.Semigroup          ((<>))
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Types      as HTTP

import           HttpTest.Spec


data MkRequestError =
  MissingVariable VariableIdentifier
  | InvalidMethod Text
  | InvalidUrl Text
  deriving (Show, Eq)


mkRequest
  :: HTTP.StdMethod
  -> Text
  -> [HTTP.Header]
  -> Maybe Text
  -> Validation [MkRequestError] HTTP.Request
mkRequest method url headers body =
  case HTTP.parseRequest (T.unpack url) of
    Nothing ->
      Failure [InvalidUrl url]
    Just req ->
      Success req { HTTP.method         = HTTP.renderStdMethod method
                  , HTTP.requestHeaders = headers
                  , HTTP.requestBody    = case body of
                                            Just t  -> HTTP.RequestBodyBS (TE.encodeUtf8 t)
                                            Nothing -> ""
                  }


performRequest :: HTTP.Request -> IO (HTTP.Response BL.ByteString)
performRequest request = do
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  HTTP.httpLbs request manager


matchResponse
  :: ResponseSpec
  -> HTTP.Response BL.ByteString
  -> Validation [ResponseMatchFailure] ()
matchResponse ResponseSpec { respSpecStatus, respSpecHeaders, respSpecBody } response =
  case result of
    [] -> Success ()
    es -> Failure es
  where
    result =
      matchResponseStatus respSpecStatus (HTTP.responseStatus response)
      <>
      matchResponseHeaders respSpecHeaders (HTTP.responseHeaders response)
      <>
      matchResponseBody respSpecBody (HTTP.responseBody response)


matchResponseStatus
  :: [ResponseSpecLiteralOrVariable]
  -> HTTP.Status
  -> [ResponseMatchFailure]
matchResponseStatus [ResponseSpecLiteral expected] actual@(HTTP.Status s msg) =
  if expected == (T.pack (show s) <> " " <> TE.decodeUtf8 msg) then
    []
  else
    [DifferentStatus expected actual]

matchResponseStatus lvs actual =
  [DifferentStatus (T.pack $ show lvs) actual]


matchResponseHeaders
  :: [[ResponseSpecLiteralOrVariable]]
  -> [HTTP.Header]
  -> [ResponseMatchFailure]
matchResponseHeaders expected actual =
  mapMaybe f expected
  where
    f [ResponseSpecLiteral lit] =
      let
        (headerName, headerVal) = T.breakOn ": " lit
        headerName' = CI.mk (TE.encodeUtf8 headerName)
        header =
          if T.null headerVal then
            (headerName', "")
          else
            (headerName', TE.encodeUtf8 $ T.drop 2 headerVal)
      in
        if header `elem` actual then
          Nothing
        else
          Just $ DifferentHeader header $ filter (\(n', _) -> n' == headerName') actual


matchResponseBody
  :: [ResponseSpecLiteralOrVariable]
  -> BL.ByteString
  -> [ResponseMatchFailure]

matchResponseBody [] actual =
  if BL.null actual then
    []
  else
    [DifferentBody Nothing [] (Just $ lbsToText actual)]

matchResponseBody expected actual =
  let
    at = lbsToText actual
  in
    match Nothing expected at
  where
    match matched [] at =
      if T.null at then
        []
      else
        [DifferentBody matched expected Nothing]

    match matched lvs@(ResponseSpecLiteral l:lvs') at =
      case T.stripPrefix l at of
        Nothing   -> nope matched lvs at
        Just rest -> match (matched <> Just l) lvs' rest

    nope matched lvs at = [DifferentBody matched lvs (Just at)]


lbsToText
  :: BL.ByteString
  -> Text
lbsToText = TL.toStrict . TLE.decodeUtf8
