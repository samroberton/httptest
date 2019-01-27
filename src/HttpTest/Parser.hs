{-# LANGUAGE OverloadedStrings #-}
module HttpTest.Parser
    ( MkRequestError
    , mkRequest
    , parseFile
    ) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.CaseInsensitive  as CI
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE

import qualified Network.HTTP.Client   as HTTP
import qualified Network.HTTP.Types    as HTTP

import           Text.Parsec

import           HttpTest.Spec


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


callSpecNameParser
  :: Parsec Text () Text
callSpecNameParser = do
  _ <- count 2 $ char '#' >> spaces
  callTitle <- manyTill anyChar newline
  return $ T.pack callTitle


requestSpecParser
  :: Parsec Text () RequestSpec
requestSpecParser = do
  _ <- string "Request:" >> many1 newline
  indent <- length <$> many1 space
  line1 <- T.pack <$> manyTill anyChar newline
  headers <- many (headerLine indent)
  pure $ RequestSpec { reqSpecLine1 = line1
                     , reqSpecHeaders = headers
                     }
  where
    headerLine :: Int -> Parsec Text () Text
    headerLine indent = try $ count indent space >> T.pack <$> manyTill anyChar newline


responseSpecParser
  :: Parsec Text () ResponseSpec
responseSpecParser = do
  _ <- string "Response:" >> many1 newline
  indent <- length <$> many1 space
  status <- statusLine
  headers <- many (headerLine indent)
  _ <- many1 newline
  body <- bodyLines indent
  pure $ ResponseSpec { respSpecStatus = status
                      , respSpecHeaders = headers
                      , respSpecBody = T.concat <$> body
                      }
  where
    statusLine :: Parsec Text () HTTP.Status
    statusLine = do
      num <- many digit
      _ <- space
      msg <- manyTill anyChar newline
      pure $ HTTP.mkStatus (read num) $ BC.pack msg

    headerLine :: Int -> Parsec Text () HTTP.Header
    headerLine indent = try $ do
      _ <- count indent space
      name <- manyTill anyChar (char ':')
      _ <- many1 space
      val <- manyTill anyChar newline
      pure (CI.mk (BC.pack name), BC.pack val)

    bodyLines :: Int -> Parsec Text () (Maybe [Text])
    bodyLines indent = optionMaybe $ many $ do
      _ <- count indent space
      l <- manyTill anyChar newline
      pure $ T.pack l


fileParser
  :: Parsec Text () (Text, RequestSpec, ResponseSpec)
fileParser = do
  call <- callSpecNameParser
  _ <- many1 newline
  req <- requestSpecParser
  _ <- many1 newline
  resp <- responseSpecParser
  return (call, req, resp)


parseFile
  :: Text
  -> Text
  -> Either ParseError (Text, RequestSpec, ResponseSpec)
parseFile srcName = parse fileParser (T.unpack srcName)
