{-# LANGUAGE OverloadedStrings #-}
module HttpTest.Parser
    ( MkRequestError
    , mkRequest
    ) where

import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types  as HTTP


data MkRequestError =
  InvalidUrl Text
  deriving (Eq, Show)

mkRequest
  :: HTTP.StdMethod
  -> Text
  -> [HTTP.Header]
  -> Maybe Text
  -> Either MkRequestError HTTP.Request
mkRequest method url headers body =
  case (HTTP.parseRequest $ T.unpack url) of
    Nothing ->
      Left (InvalidUrl url)
    Just req ->
      return req { HTTP.method         = HTTP.renderStdMethod method
                 , HTTP.requestHeaders = headers
                 , HTTP.requestBody    = case body of
                                           Just t  -> HTTP.RequestBodyBS (TE.encodeUtf8 t)
                                           Nothing -> ""
                 }
