{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module HttpTest.MarkdownParser
    ( parseFile
    ) where

import qualified Debug.Trace

import qualified Data.ByteString.Char8 as BC
import           Data.Functor          (void)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE

import qualified Network.HTTP.Client   as HTTP
import qualified Network.HTTP.Types    as HTTP

import           Text.Parsec
import           Text.Parsec.Text

import           HttpTest.Spec



indent :: Parser String
indent = string "    "


-- | @endOfLine@, but returns either the LF or CRLF it parsed, not always LF.
endOfLine'
  :: (Stream s m Char)
  => ParsecT s u m String
endOfLine' = try (string "\r\n") <|> string "\n"


variableIdentifier :: Parser VariableIdentifier
variableIdentifier = VariableIdentifier . T.pack <$> many1 alphaNum


callSpecNameParser :: Parser Text
callSpecNameParser = T.pack <$> (string "##" *> spaces *> manyTill anyChar endOfLine)


requestSpecLiteralOrVariablesParser :: Parser [RequestSpecLiteralOrVariable]
requestSpecLiteralOrVariablesParser =
  many (variable <|> literal)
  where
    variable :: Parser RequestSpecLiteralOrVariable
    variable = RequestSpecVariable <$> (string "${" *> variableIdentifier <* string "}")

    literal :: Parser RequestSpecLiteralOrVariable
    literal = do
      lit <- manyTill anyChar endOfLiteral
      case lit of
        -- `unexpected` => terminate the `many` in the main function body
        []   -> unexpected "empty literal"
        lit' -> pure $ RequestSpecLiteral (T.pack lit')

    endOfLiteral :: Parser ()
    endOfLiteral = lookAhead $ void (string "${") <|> void endOfLine



-- | Take a parser for a single line of a request or response body, without
-- indent, and produce a parser for a (multi-line) body where the body is
-- indented, and where the parser will stop consuming input when it sees a
-- newline with non-indented text.
--
-- Like the rest of the HTTP request, the request/response body is indented by
-- four spaces, but we allow non-trailing empty lines within the body to omit
-- the indent (to avoid mandating lines with trailing whitespace).
--
-- Multiple consecutive newlines (without indent) immediately before a
-- non-indented line will be ignored.
--
-- Parsers for request/response body lines produce a list of tokens `[a]`, where
-- each token `a` is eg a literal or a variable usage.  `bodyLinesParser` will
-- parse each line individually, but use `combine` to allow you to combine the
-- end of one line, the newline(s), and the beginning of the next line into one
-- single token (eg one literal), or two tokens (eg one literal and one variable
-- usage, or vice versa), or leave them as three separate tokens (eg variable
-- usage, literal newline, variable usage).
bodyLinesParser
  :: forall a. Show a =>
     Parser [a]
  -> (Maybe a -> [String] -> Maybe a -> [a])
  -- ^ last `a` of prev line if any -> newlines -> first `a` of next line if any
  -> Parser [a]
bodyLinesParser p combine = go Nothing
    where
      go
        :: Maybe a
        -> Parser [a]
      go lastTokenOfPrevLine = do
        nls <- manyNewlines
        go' lastTokenOfPrevLine nls <|> return (finish lastTokenOfPrevLine nls)

      finish
        :: Maybe a
        -> [(Bool, String)]
        -> [a]
      finish lastTokenOfPrevLine nls =
        let
          nls' = reverse $ drop 1 $ dropWhile (not . fst) (reverse nls)
        in
          combine lastTokenOfPrevLine (snd <$> nls') Nothing

      go'
        :: Maybe a
        -> [(Bool, String)]
        -> Parser [a]
      go' lastTokenOfPrevLine nls = do
        -- Recursion termination condition is that `indentedLine`
        -- fails. This relies on us already having consumed `manyNewlines`
        -- immediately prior to calling `go`.
        line <- indent *> p
        case line of
          [] ->
            unexpected "no tokens for non-empty line"
          [t] ->
            let
              t' = combine lastTokenOfPrevLine (snd <$> nls) (Just t)
            in
              (initOrEmpty t' <>) <$> go (lastMaybe t')
          (t:ts) -> do
            rest <- go (lastMaybe ts)
            return $ combine lastTokenOfPrevLine (snd <$> nls) (Just t) <> initOrEmpty ts <> rest

      manyNewlines
        :: Parser [(Bool, String)]
      manyNewlines = many1Newlines <|> pure []

      many1Newlines
        :: Parser [(Bool, String)]
      many1Newlines = do
        nl <- endOfLine'
        nls <- many optionallyIndentedNewline
        return $ (True, nl) : nls

      optionallyIndentedNewline
        :: Parser (Bool, String)
      optionallyIndentedNewline = try $ do
        i <- optionMaybe indent
        nl <- endOfLine'
        return $ case i of
          Nothing -> (False, nl)
          Just _  -> (True, nl)


requestSpecParser :: Parser RequestSpec
requestSpecParser = do
  _ <- string "Request:"
  _ <- count 2 endOfLine
  _ <- indent
  line1 <- requestSpecLiteralOrVariablesParser
  _ <- endOfLine
  headers <- many headerLine
  _ <- endOfLine
  body <- bodyLinesParser requestSpecLiteralOrVariablesParser combineBodyComponents
  pure $ RequestSpec { reqSpecLine1   = line1
                     , reqSpecHeaders = headers
                     , reqSpecBody    = body
                     }
  where
    headerLine :: Parser [RequestSpecLiteralOrVariable]
    headerLine = try (indent *> requestSpecLiteralOrVariablesParser <* endOfLine)

    combineBodyComponents :: Maybe RequestSpecLiteralOrVariable
                          -> [String]
                          -> Maybe RequestSpecLiteralOrVariable
                          -> [RequestSpecLiteralOrVariable]
    combineBodyComponents Nothing [] Nothing = []
    combineBodyComponents Nothing nls Nothing = [RequestSpecLiteral $ T.pack (concat nls)]
    combineBodyComponents Nothing nls (Just (RequestSpecLiteral l)) = [RequestSpecLiteral (T.pack (concat nls) <> l)]
    combineBodyComponents (Just (RequestSpecLiteral l))  nls Nothing = [RequestSpecLiteral (l <> T.pack (concat nls))]
    combineBodyComponents (Just (RequestSpecLiteral l1)) nls (Just (RequestSpecLiteral l2)) = [RequestSpecLiteral (l1 <> T.pack (concat nls) <> l2)]
    combineBodyComponents (Just (RequestSpecLiteral l))  nls (Just v@(RequestSpecVariable _)) = [RequestSpecLiteral (l <> T.pack (concat nls)), v]
    combineBodyComponents (Just v@(RequestSpecVariable _)) nls (Just (RequestSpecLiteral l)) = [v, RequestSpecLiteral (T.pack (concat nls) <> l)]


responseSpecLiteralOrVariablesParser :: Parser [ResponseSpecLiteralOrVariable]
responseSpecLiteralOrVariablesParser =
  many (try variableDefn <|> try variableUsage <|> literal)
  where
    variableDefn :: Parser ResponseSpecLiteralOrVariable
    variableDefn = do
      var <- string "${{" *> variableIdentifier
      _ <- spaces *> string ":=" <* spaces
      -- FIXME prohibit newlines in regexes?  escaped "/}}"?
      regex <- char '/' *> manyTill anyChar (string "/}}")
      pure $ ResponseSpecVariableExtraction $ ExtractedVariable var (T.pack regex)

    variableUsage :: Parser ResponseSpecLiteralOrVariable
    variableUsage = ResponseSpecVariableUsage <$> (string "${" *> variableIdentifier <* char '}')

    literal :: Parser ResponseSpecLiteralOrVariable
    literal = do
      lit <- manyTill anyChar (lookAhead $ void (string "${") <|> void endOfLine)
      case lit of
        []   -> unexpected "empty literal"
        lit' -> pure $ ResponseSpecLiteral (T.pack lit')


responseSpecParser :: Parser ResponseSpec
responseSpecParser = do
  _ <- string "Response:"
  _ <- count 2 endOfLine
  status <- statusLine
  headers <- many headerLine
  _ <- endOfLine
  body <- bodyLinesParser responseSpecLiteralOrVariablesParser combineBodyComponents
  pure $ ResponseSpec { respSpecStatus  = status
                      , respSpecHeaders = headers
                      , respSpecBody    = body
                      }
  where
    statusLine :: Parser [ResponseSpecLiteralOrVariable]
    statusLine = indent *> responseSpecLiteralOrVariablesParser <* endOfLine

    headerLine :: Parser [ResponseSpecLiteralOrVariable]
    headerLine = try (indent *> responseSpecLiteralOrVariablesParser <* endOfLine)

    combineBodyComponents :: Maybe ResponseSpecLiteralOrVariable
                          -> [String]
                          -> Maybe ResponseSpecLiteralOrVariable
                          -> [ResponseSpecLiteralOrVariable]
    combineBodyComponents Nothing [] Nothing = []
    combineBodyComponents Nothing nls Nothing = [ResponseSpecLiteral $ T.pack (concat nls)]
    combineBodyComponents Nothing nls (Just (ResponseSpecLiteral l)) = [ResponseSpecLiteral (T.pack (concat nls) <> l)]
    combineBodyComponents (Just (ResponseSpecLiteral l))  nls Nothing = [ResponseSpecLiteral (l <> T.pack (concat nls))]
    combineBodyComponents (Just (ResponseSpecLiteral l1)) nls (Just (ResponseSpecLiteral l2)) = [ResponseSpecLiteral (l1 <> T.pack (concat nls) <> l2)]
    combineBodyComponents (Just (ResponseSpecLiteral l))  nls (Just v@(ResponseSpecVariableUsage _)) = [ResponseSpecLiteral (l <> T.pack (concat nls)), v]
    combineBodyComponents (Just (ResponseSpecLiteral l))  nls (Just v@(ResponseSpecVariableExtraction _)) = [ResponseSpecLiteral (l <> T.pack (concat nls)), v]
    combineBodyComponents (Just v@(ResponseSpecVariableUsage _)) nls (Just (ResponseSpecLiteral l)) = [v, ResponseSpecLiteral (T.pack (concat nls) <> l)]
    combineBodyComponents (Just v@(ResponseSpecVariableExtraction _)) nls (Just (ResponseSpecLiteral l)) = [v, ResponseSpecLiteral (T.pack (concat nls) <> l)]


fileParser :: Parser [(Text, RequestSpec, ResponseSpec)]
fileParser = many $ do
  call <- callSpecNameParser
  _ <- many1 endOfLine
  req <- requestSpecParser
  _ <- many endOfLine
  resp <- responseSpecParser
  _ <- many endOfLine
  return (call, req, resp)


parseFile :: Text
          -> Text
          -> Either ParseError [(Text, RequestSpec, ResponseSpec)]
parseFile srcName = parse fileParser (T.unpack srcName)


-- Util functions.

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe [a] = Just a
lastMaybe (_:as) = lastMaybe as


initOrEmpty :: [a] -> [a]
initOrEmpty [] = []
initOrEmpty [a] = []
initOrEmpty (a:as) = a : initOrEmpty as
