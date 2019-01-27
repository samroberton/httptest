module HttpTest.Spec where

import           Data.Text          (Text)
import qualified Network.HTTP.Types as HTTP


data RequestSpec =
  RequestSpec { reqSpecLine1   :: Text
              , reqSpecHeaders :: [Text]
              }
  deriving (Show, Eq)


data ResponseSpec =
  ResponseSpec { respSpecStatus  :: HTTP.Status
               , respSpecHeaders :: [HTTP.Header]
               , respSpecBody    :: Maybe Text
               }
  deriving (Show, Eq)


data ResponseMatchFailure =
  DifferentStatus HTTP.Status HTTP.Status
  | DifferentHeader HTTP.Header [HTTP.Header]
  | DifferentBody (Maybe Text) (Maybe Text)
  deriving (Show, Eq)
