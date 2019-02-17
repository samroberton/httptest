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



-- | Take a parser for a list of tokens, where parsing the tokens will never
-- consume a newline, and produce a parser for multi-line indented text, where
-- the parser will stop consuming input when it sees a newline with non-indented
-- text.
--
-- Our HTTP requests/responses are indented by four spaces, but we allow
-- non-trailing empty lines to omit the indent (to avoid mandating lines with
-- trailing whitespace).
--
-- Multiple consecutive newlines (without indent) immediately before a
-- non-indented line will be ignored.
--
-- Parsers for request/response body lines produce a list of tokens, where each
-- tokenis eg a literal or a variable usage.  `indentedLinesParser` will parse
-- each line individually, but use `combine` to allow you to combine the end of
-- one line, the newline(s), and the beginning of the next line into one single
-- token (eg one literal), or two tokens (eg one literal and one variable usage,
-- or vice versa), or leave them as three separate tokens (eg variable usage,
-- literal newline, variable usage).
indentedLinesParser
  :: forall a.
     Parser [a]
  -> (Maybe a -> [String] -> Maybe a -> [a])
  -- ^ last `a` of prev line if any -> newlines -> first `a` of next line if any
  -> Parser [a]
indentedLinesParser p combine = go Nothing
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


requestSpecParser
  :: Parser RequestSpec
requestSpecParser =
  RequestSpec <$> ( string "Request:"
                    *> count 2 endOfLine
                    *> many endOfLine
                    *> indentedLinesParser requestSpecLiteralOrVariablesParser combine
                  )
  where
    combine
      :: Maybe RequestSpecLiteralOrVariable
      -> [String]
      -> Maybe RequestSpecLiteralOrVariable
      -> [RequestSpecLiteralOrVariable]
    combine (Just a) [] Nothing = [a]
    combine Nothing nls Nothing = [RequestSpecLiteral $ T.pack (concat nls)]
    combine Nothing nls (Just (RequestSpecLiteral l)) = [RequestSpecLiteral (T.pack (concat nls) <> l)]
    combine Nothing nls (Just v@(RequestSpecVariable _)) = [RequestSpecLiteral (T.pack (concat nls)), v]
    combine (Just (RequestSpecLiteral l))  nls Nothing = [RequestSpecLiteral (l <> T.pack (concat nls))]
    combine (Just (RequestSpecLiteral l1)) nls (Just (RequestSpecLiteral l2)) = [RequestSpecLiteral (l1 <> T.pack (concat nls) <> l2)]
    combine (Just (RequestSpecLiteral l))  nls (Just v@(RequestSpecVariable _)) = [RequestSpecLiteral (l <> T.pack (concat nls)), v]
    combine (Just v@(RequestSpecVariable _)) nls Nothing = [v, RequestSpecLiteral (T.pack (concat nls))]
    combine (Just v@(RequestSpecVariable _)) nls (Just (RequestSpecLiteral l)) = [v, RequestSpecLiteral (T.pack (concat nls) <> l)]
    combine (Just v1@(RequestSpecVariable _)) nls (Just v2@(RequestSpecVariable _)) = [v1, RequestSpecLiteral (T.pack (concat nls)), v2]


responseSpecLiteralOrVariablesParser
  :: Parser [ResponseSpecLiteralOrVariable]
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


responseSpecParser
  :: Parser ResponseSpec
responseSpecParser =
  ResponseSpec <$> ( string "Response:"
                     *> count 2 endOfLine
                     *> many endOfLine
                     *> indentedLinesParser responseSpecLiteralOrVariablesParser combine
                   )
  where
    combine
      :: Maybe ResponseSpecLiteralOrVariable
      -> [String]
      -> Maybe ResponseSpecLiteralOrVariable
      -> [ResponseSpecLiteralOrVariable]
    combine (Just a) [] Nothing = [a]
    combine Nothing nls Nothing = [ResponseSpecLiteral $ T.pack (concat nls)]
    combine Nothing nls (Just (ResponseSpecLiteral l)) = [ResponseSpecLiteral (T.pack (concat nls) <> l)]
    combine (Just (ResponseSpecLiteral l))  nls Nothing = [ResponseSpecLiteral (l <> T.pack (concat nls))]
    combine (Just (ResponseSpecLiteral l1)) nls (Just (ResponseSpecLiteral l2)) = [ResponseSpecLiteral (l1 <> T.pack (concat nls) <> l2)]
    combine (Just (ResponseSpecLiteral l))  nls (Just v@(ResponseSpecVariableUsage _)) = [ResponseSpecLiteral (l <> T.pack (concat nls)), v]
    combine (Just (ResponseSpecLiteral l))  nls (Just v@(ResponseSpecVariableExtraction _)) = [ResponseSpecLiteral (l <> T.pack (concat nls)), v]
    combine (Just v@(ResponseSpecVariableUsage _)) nls (Just (ResponseSpecLiteral l)) = [v, ResponseSpecLiteral (T.pack (concat nls) <> l)]
    combine (Just v@(ResponseSpecVariableExtraction _)) nls (Just (ResponseSpecLiteral l)) = [v, ResponseSpecLiteral (T.pack (concat nls) <> l)]


fileParser
  :: Parser [(Text, RequestSpec, ResponseSpec)]
fileParser = many $ do
  call <- callSpecNameParser
  _ <- many1 endOfLine
  req <- requestSpecParser
  _ <- many endOfLine
  resp <- responseSpecParser
  _ <- many endOfLine
  return (call, req, resp)


parseFile
  :: Text
  -> Text
  -> Either ParseError [(Text, RequestSpec, ResponseSpec)]
parseFile srcName = parse fileParser (T.unpack srcName)


-- Util functions.

lastMaybe
  :: [a]
  -> Maybe a
lastMaybe [] = Nothing
lastMaybe [a] = Just a
lastMaybe (_:as) = lastMaybe as


initOrEmpty
  :: [a]
  -> [a]
initOrEmpty [] = []
initOrEmpty [_] = []
initOrEmpty (a:as) = a : initOrEmpty as
