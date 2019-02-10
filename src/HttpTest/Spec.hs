module HttpTest.Spec where

import           Data.Text          (Text)
import qualified Network.HTTP.Types as HTTP


newtype VariableIdentifier =
  VariableIdentifier { unVariableIdentifier :: Text }
  deriving (Show, Eq)


data ExtractedVariable =
  ExtractedVariable { extractedVariableIdentifier :: VariableIdentifier
                    , extractedVariableRegex      :: Text
                    }
  deriving (Show, Eq)


data RequestSpecLiteralOrVariable =
  RequestSpecLiteral Text
  | RequestSpecVariable VariableIdentifier
  deriving (Show, Eq)


data RequestSpec =
  RequestSpec { reqSpecLine1   :: [RequestSpecLiteralOrVariable]
              , reqSpecHeaders :: [[RequestSpecLiteralOrVariable]]
              , reqSpecBody    :: [RequestSpecLiteralOrVariable]
              }
  deriving (Show, Eq)


data ResponseSpecLiteralOrVariable =
  ResponseSpecLiteral Text
  | ResponseSpecVariableUsage VariableIdentifier
  | ResponseSpecVariableExtraction ExtractedVariable
  deriving (Show, Eq)


data ResponseSpec =
  ResponseSpec { respSpecStatus  :: HTTP.Status
               , respSpecHeaders :: [[ResponseSpecLiteralOrVariable]]
               , respSpecBody    :: [ResponseSpecLiteralOrVariable]
               }
  deriving (Show, Eq)


data ResponseMatchFailure =
  DifferentStatus HTTP.Status HTTP.Status
  | DifferentHeader HTTP.Header [HTTP.Header]
  | DifferentBody (Maybe Text) [ResponseSpecLiteralOrVariable] (Maybe Text)
  deriving (Show, Eq)
