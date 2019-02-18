module HttpTest.Spec where

import qualified Data.Map           as M
import           Data.Text          (Text)
import qualified Network.HTTP.Types as HTTP


newtype VariableIdentifier =
  VariableIdentifier { unVariableIdentifier :: Text }
  deriving (Show, Eq, Ord)


data ExtractedVariable =
  ExtractedVariable { extractedVariableIdentifier :: VariableIdentifier
                    , extractedVariableRegex      :: Text
                    }
  deriving (Show, Eq)


data RequestSpecLiteralOrVariable =
  RequestSpecLiteral Text
  | RequestSpecVariable VariableIdentifier
  deriving (Show, Eq)


newtype RequestSpec =
  RequestSpec { unRequestSpec :: [RequestSpecLiteralOrVariable] }
  deriving (Show, Eq)


data ResponseSpecLiteralOrVariable =
  ResponseSpecLiteral Text
  | ResponseSpecVariableUsage VariableIdentifier
  | ResponseSpecVariableExtraction ExtractedVariable
  deriving (Show, Eq)


newtype ResponseSpec =
  ResponseSpec { unResponseSpec :: [ResponseSpecLiteralOrVariable] }
  deriving (Show, Eq)


newtype Environment =
  Environment { unEnvironment :: M.Map VariableIdentifier Text }
  deriving (Show, Eq)


data ResponseMatchFailure =
  MissingResponseVariable VariableIdentifier
  | UnparseableResponseSpec Text
  | DifferentStatus Text HTTP.Status
  | DifferentHeader HTTP.Header [HTTP.Header]
  | DifferentBody (Maybe Text) (Maybe Text)
  deriving (Show, Eq)


data MkRequestError =
  MissingVariable VariableIdentifier
  | InvalidMethod Text
  | InvalidUrl Text
  deriving (Show, Eq)
