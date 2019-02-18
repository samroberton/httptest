{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module HttpTest.Runner
    ( matchResponse
    , mkRequest
    , performRequest
    ) where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.CaseInsensitive    as CI
import           Data.Either.Validation  (Validation (..))
import qualified Data.Map                as M
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




mkRequest
  :: HTTP.StdMethod
  -> Text
  -> [HTTP.Header]
  -> Maybe Text
  -> Validation [MessageCreateError] HTTP.Request
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
  :: MessageSpec
  -> Environment
  -> HTTP.Response BL.ByteString
  -> Validation [MessageMatchFailure] ()
matchResponse (MessageSpec ts _) env response =
  case splitResponse ts env of
    Failure f ->
      Failure f
    Success (status, headers, body) ->
      let
        result = matchResponseStatus status (HTTP.responseStatus response)
                 <>
                 matchResponseHeaders headers (HTTP.responseHeaders response)
                 <>
                 matchResponseBody body (HTTP.responseBody response)
      in
        case result of
          [] -> Success ()
          es -> Failure es


substituteResp
  :: Environment
  -> [MessageToken]
  -> Validation [MessageMatchFailure] Text
substituteResp (Environment env) = foldl go (Success "")
  where
    go (Success t) (MessageTokenLiteral  t') = Success (t <> t')
    go (Success t) (MessageTokenVariable v)  =
      case M.lookup v env of
        Just t' -> Success (t <> t')
        Nothing -> Failure [MissingMatchVariable v]

    go r@(Failure _)   (MessageTokenLiteral  _) = r
    go r@(Failure mvs) (MessageTokenVariable v) =
      case M.lookup v env of
        Just _  -> r
        Nothing -> Failure (mvs <> [MissingMatchVariable v])


splitResponse
  :: [MessageToken]
  -> Environment
  -> Validation [MessageMatchFailure] (Text, [Text], Maybe Text)
splitResponse lvs env =
  case substituteResp env lvs of
    Failure f -> Failure f
    Success t ->
      let
        (preamble, body') = T.breakOn "\n\n" t
      in
        case (T.splitOn "\n" preamble, T.stripPrefix "\n\n" body') of
          ([], _) ->
            error "T.splitOn returned empty list!"
          (statusLine:headers, Nothing) ->
            if T.null body' then
              Success (statusLine, headers, Nothing)
            else
              Failure [UnparseableMessage t]
          (statusLine:headers, Just "") ->
            Success (statusLine, headers, Nothing)
          (statusLine:headers, Just b)  ->
            Success (statusLine, headers, Just b)


matchResponseStatus
  :: Text
  -> HTTP.Status
  -> [MessageMatchFailure]
matchResponseStatus expected actual@(HTTP.Status s msg) =
  if expected == (T.pack (show s) <> " " <> TE.decodeUtf8 msg) then
    []
  else
    [DifferentStatus expected actual]


matchResponseHeaders
  :: [Text]
  -> [HTTP.Header]
  -> [MessageMatchFailure]
matchResponseHeaders expected actual =
  mapMaybe f expected
  where
    f t =
      let
        (headerName, headerVal) = T.breakOn ": " t
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
  :: Maybe Text
  -> BL.ByteString
  -> [MessageMatchFailure]

matchResponseBody expected actual =
  let
    actual' = if BL.null actual then Nothing else Just (lbsToText actual)
  in
    if expected == actual' then
      []
    else
      [DifferentBody expected (Just $ lbsToText actual)]


lbsToText
  :: BL.ByteString
  -> Text
lbsToText = TL.toStrict . TLE.decodeUtf8
