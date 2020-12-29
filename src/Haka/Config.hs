{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Haka.Config
  ( API,
    server,
  )
where

import Control.Exception.Safe (throw)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Int (Int64)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Time (addDays, diffDays)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import GHC.Generics
import Haka.AesonHelpers (noPrefixOptions)
import qualified Haka.DatabaseOperations as DbOps
import Haka.Errors (missingAuthError)
import qualified Haka.Errors as Err
import Haka.Types (ApiToken (..), AppM, ProjectStatRow (..), pool)
import Haka.Utils (defaultLimit, sum')
import Polysemy (runM)
import Polysemy.Error (runError)
import Polysemy.IO (embedToMonadIO)
import PostgreSQL.Binary.Data (Scientific)
import Servant

-- data ConfigRequestType = AddType | RemoveType | UpdateType
--   deriving (Eq, Show, Generic)

-- instance FromJSON ConfigRequestType where
--   parseJSON (A.String p) = case p of
--     "add" -> return AddType
--     "remove" -> return RemoveType
--     "update" -> return UpdateType
--     _ -> fail "Request can only be one of ['add', 'remove', 'update']"
--   parseJSON _ = fail "Expected a string value"

-- instance ToJSON ConfigRequestType where
--   toJSON AddType = A.String "add"
--   toJSON RemoveType = A.String "remove"
--   toJSON UpdateType = A.String "update"

-- data ClassConfigReq = ClassConfigReq
--   { -- | The class name.
--     name :: Text,
--     -- | Request Type
--     action :: ConfigRequestType
--   }
--   deriving (Show, Generic)

-- instance FromJSON ClassConfigReq where
--   parseJSON = genericParseJSON convertReservedWords

-- instance ToJSON ClassConfigReq where
--   toJSON = genericToJSON convertReservedWords


data ConfigResponse = ConfigResponse
  { -- | Status
    status :: Text
  }
  deriving(Show, Generic)

defaultConfigResponse :: ConfigResponse
defaultConfigResponse =
  ConfigResponse
    {
      status="Completed."
    }

instance ToJSON ConfigResponse
instance FromJSON ConfigResponse

type API = ClassConfig

server = classConfigHandler


-- Class Configuration
type ClassConfig =
  "api"
    :> "v1"
    :> "users"
    :> "current"
    :> "config"
    :> "classes"
    :> Capture "class-name" Text
    :> Capture "req-type" Text
    :> Header "Authorization" ApiToken
    :> Get '[JSON] ConfigResponse


classConfigHandler ::
  Text ->
  Text ->
  Maybe ApiToken ->
  AppM ConfigResponse
classConfigHandler _ _ Nothing = throw missingAuthError
classConfigHandler className reqType (Just token) = do
  p <- asks pool
  res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.updateClassConfiguration p token className reqType

  output <- either Err.logError pure res

  return defaultConfigResponse

