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


data MessageToken =
  MessageTokenLiteral Text
  | MessageTokenVariable VariableIdentifier
  deriving (Show, Eq)


newtype Regex =
  Regex { unRegex :: Text }
  deriving (Show, Eq)

data MessageSpec =
  MessageSpec { messageSpecTokens      :: [MessageToken]
              , messageSpecExtractions :: [(VariableIdentifier, Regex)]
              }
  deriving (Show, Eq)


newtype Environment =
  Environment { unEnvironment :: M.Map VariableIdentifier Text }
  deriving (Show, Eq)


data MessageMatchFailure =
  MissingMatchVariable VariableIdentifier
  | UnparseableMessage Text
  | DifferentStatus Text HTTP.Status
  | DifferentHeader HTTP.Header [HTTP.Header]
  | DifferentBody (Maybe Text) (Maybe Text)
  deriving (Show, Eq)


data MessageCreateError =
  MissingUsedVariable VariableIdentifier
  | InvalidMethod Text
  | InvalidUrl Text
  deriving (Show, Eq)
