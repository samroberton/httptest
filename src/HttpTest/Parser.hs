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
import           Text.Parsec.Text

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


indent
  :: Parser String
indent = string "    "


callSpecNameParser
  :: Parser Text
callSpecNameParser = do
  _ <- string "##" >> spaces
  callTitle <- manyTill anyChar newline
  return $ T.pack callTitle


requestSpecParser
  :: Parser RequestSpec
requestSpecParser = do
  _ <- string "Request:"
  _ <- count 2 newline
  _ <- indent
  line1 <- T.pack <$> manyTill anyChar newline
  headers <- many headerLine
  _ <- newline
  body <- bodyLines
  pure $ RequestSpec { reqSpecLine1   = line1
                     , reqSpecHeaders = headers
                     , reqSpecBody    = case body of
                                          [] -> Nothing
                                          bs -> Just $ T.intercalate "\n" bs
                     }
  where
    headerLine :: Parser Text
    headerLine = try $ indent >> T.pack <$> manyTill anyChar newline

    bodyLines :: Parser [Text]
    bodyLines = many $ try $ do
      _ <- indent
      l <- manyTill anyChar newline
      pure $ T.pack l


responseSpecParser
  :: Parser ResponseSpec
responseSpecParser = do
  _ <- string "Response:"
  _ <- count 2 newline
  status <- statusLine
  headers <- many headerLine
  _ <- newline
  body <- bodyLines
  pure $ ResponseSpec { respSpecStatus = status
                      , respSpecHeaders = headers
                      , respSpecBody = case body of
                                         [] -> Nothing
                                         bs -> Just $ T.intercalate "\n" bs
                      }
  where
    statusLine :: Parser HTTP.Status
    statusLine = do
      _ <- indent
      num <- many digit
      _ <- space
      msg <- manyTill anyChar newline
      pure $ HTTP.mkStatus (read num) $ BC.pack msg

    headerLine :: Parser HTTP.Header
    headerLine = try $ do
      _ <- indent
      name <- manyTill anyChar (char ':')
      _ <- many1 space
      val <- manyTill anyChar newline
      pure (CI.mk (BC.pack name), BC.pack val)

    bodyLines :: Parser [Text]
    bodyLines = many $ try $ do
      _ <- indent
      l <- manyTill anyChar newline
      pure $ T.pack l


fileParser
  :: Parser [(Text, RequestSpec, ResponseSpec)]
fileParser = many $ do
  call <- callSpecNameParser
  _ <- many1 newline
  req <- requestSpecParser
  _ <- many newline
  resp <- responseSpecParser
  _ <- many newline
  return (call, req, resp)


parseFile
  :: Text
  -> Text
  -> Either ParseError [(Text, RequestSpec, ResponseSpec)]
parseFile srcName = parse fileParser (T.unpack srcName)
