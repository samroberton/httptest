{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module HttpTest.MarkdownParser
    ( parseFile
    ) where


import           Data.Functor          (void)
import           Data.Text             (Text)
import qualified Data.Text             as T

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
variableIdentifier = VariableIdentifier . T.pack <$> (spaces *> many1 alphaNum <* spaces)


callSpecNameParser :: Parser Text
callSpecNameParser = T.pack <$> (string "##" *> spaces *> manyTill anyChar endOfLine)


messageTokenParser :: Parser [MessageToken]
messageTokenParser =
  many (variable <|> literal)
  where
    variable :: Parser MessageToken
    variable = MessageTokenVariable <$> (string "${" *> variableIdentifier <* string "}")

    literal :: Parser MessageToken
    literal = do
      lit <- manyTill anyChar endOfLiteral
      case lit of
        -- `unexpected` => terminate the `many` in the main function body
        []   -> unexpected "empty literal"
        lit' -> pure $ MessageTokenLiteral (T.pack lit')

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


messageSpecParser
  :: String -- ^ "Request" or "Response"
  -> Parser MessageSpec
messageSpecParser msgName = do
  msg <- string "###"
         *> spaces
         *> string msgName
         *> count 2 endOfLine
         *> many endOfLine
         *> indentedLinesParser messageTokenParser combine
  extractions <- optionMaybe $ try $ string "Extract:"
                                     *> many1 endOfLine
                                     *> many extractionParser
  pure $ MessageSpec { messageSpecTokens      = msg
                     , messageSpecExtractions = maybe [] id extractions
                     }
  where
    combine
      :: Maybe MessageToken
      -> [String]
      -> Maybe MessageToken
      -> [MessageToken]
    combine (Just a) [] Nothing = [a]
    combine Nothing nls Nothing = [MessageTokenLiteral $ T.pack (concat nls)]
    combine Nothing nls (Just (MessageTokenLiteral l)) = [MessageTokenLiteral (T.pack (concat nls) <> l)]
    combine Nothing nls (Just v@(MessageTokenVariable _)) = [MessageTokenLiteral (T.pack (concat nls)), v]
    combine (Just (MessageTokenLiteral l))  nls Nothing = [MessageTokenLiteral (l <> T.pack (concat nls))]
    combine (Just (MessageTokenLiteral l1)) nls (Just (MessageTokenLiteral l2)) = [MessageTokenLiteral (l1 <> T.pack (concat nls) <> l2)]
    combine (Just (MessageTokenLiteral l))  nls (Just v@(MessageTokenVariable _)) = [MessageTokenLiteral (l <> T.pack (concat nls)), v]
    combine (Just v@(MessageTokenVariable _)) nls Nothing = [v, MessageTokenLiteral (T.pack (concat nls))]
    combine (Just v@(MessageTokenVariable _)) nls (Just (MessageTokenLiteral l)) = [v, MessageTokenLiteral (T.pack (concat nls) <> l)]
    combine (Just v1@(MessageTokenVariable _)) nls (Just v2@(MessageTokenVariable _)) = [v1, MessageTokenLiteral (T.pack (concat nls)), v2]

    extractionParser
      :: Parser (VariableIdentifier, Regex)
    extractionParser = do
      _ <- char '*'
           *> spaces
           *> char '`'
      var <- variableIdentifier
      _ <- spaces *> char '~' <* spaces
      re <- regexParser
      _ <- char '`' <* endOfLine
      pure (var, re)

    regexParser
      :: Parser Regex
     -- FIXME allow escaped backslashes
    regexParser = Regex . T.pack <$> (char '/' *> manyTill anyChar (char '/'))


fileParser
  :: Parser [(Text, MessageSpec, MessageSpec)]
fileParser = many $ do
  call <- callSpecNameParser
  _ <- many1 endOfLine
  req <- messageSpecParser "Request"
  _ <- many endOfLine
  resp <- messageSpecParser "Response"
  _ <- many endOfLine
  return (call, req, resp)


parseFile
  :: Text
  -> Text
  -> Either ParseError [(Text, MessageSpec, MessageSpec)]
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
