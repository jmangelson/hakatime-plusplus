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
import Haka.Types (ApiToken (..), AppM, ProjectStatRow (..), ConfigPayload (..), MajorCategoriesRow (..), SubCategoriesRow (..), SubSubCategoriesRow (..), pool)
import Haka.Utils (defaultLimit, sum')
import Polysemy (runM)
import Polysemy.Error (runError)
import Polysemy.IO (embedToMonadIO)
import PostgreSQL.Binary.Data (Scientific)
import Servant



-- Config Response
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


-- Combined API/Server
type API = Configuration :<|> ClassConfig :<|> MajCatConfig :<|> TaskStateConfig :<|> GoalClassConfig :<|> SubCatConfig :<|> SubSubCatConfig

server = configurationHandler :<|> classConfigHandler :<|> majCatConfigHandler :<|> taskStateConfigHandler :<|> goalClassConfigHandler :<|> subCatConfigHandler :<|> subSubCatConfigHandler


-- Configuration Payload API and Server
type Configuration =
  "api"
    :> "v1"
    :> "users"
    :> "current"
    :> "configuration"
    :> Header "Authorization" ApiToken
    :> Get '[JSON] ConfigPayload


configurationHandler ::
  Maybe ApiToken ->
  AppM ConfigPayload
configurationHandler Nothing = throw missingAuthError
configurationHandler (Just token) = do
  p <- asks pool
  classes_res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.getClassList p token
        
  classes_list <- either Err.logError pure classes_res
  
  task_state_res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.getTaskStateList p token
        
  task_state_list <- either Err.logError pure task_state_res

  goal_class_res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.getGoalClassList p token
        
  goal_class_list <- either Err.logError pure goal_class_res

  maj_cat_res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.getMajCatList p token
        
  maj_cat_list <- either Err.logError pure maj_cat_res

  sub_cat_res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.getSubCatList p token
        
  sub_cat_list <- either Err.logError pure sub_cat_res

  sub_sub_cat_res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.getSubSubCatList p token
        
  sub_sub_cat_list <- either Err.logError pure sub_sub_cat_res


  return $ toConfigPayload classes_list maj_cat_list sub_cat_list sub_sub_cat_list task_state_list goal_class_list

toConfigPayload :: [Text] -> [MajorCategoriesRow] -> [SubCategoriesRow] -> [SubSubCategoriesRow] -> [Text] -> [Text] -> ConfigPayload
toConfigPayload classes_list maj_cat_list sub_cat_list sub_sub_cat_list task_state_list goal_class_list =
  ConfigPayload
  {
    classes = classes_list,
    major_categories = maj_cat_list,
    sub_categories = sub_cat_list,
    sub_sub_categories = sub_sub_cat_list, 
    task_states = task_state_list,
    goal_classes = goal_class_list
  }


-- Class Config API and Server
type ClassConfig =
  "api"
    :> "v1"
    :> "users"
    :> "current"
    :> "config"
    :> "classes"
    :> Capture "req-type" Text
    :> Capture "class-name" Text
    :> Header "Authorization" ApiToken
    :> Get '[JSON] ConfigResponse


classConfigHandler ::
  Text ->
  Text ->
  Maybe ApiToken ->
  AppM ConfigResponse
classConfigHandler _ _ Nothing = throw missingAuthError
classConfigHandler reqType className (Just token) = do
  p <- asks pool
  res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.updateClassConfiguration p token reqType className

  output <- either Err.logError pure res  

  return defaultConfigResponse


-- TaskState Config API and Server
type TaskStateConfig =
  "api"
    :> "v1"
    :> "users"
    :> "current"
    :> "config"
    :> "taskstate"
    :> Capture "req-type" Text
    :> Capture "task-state-name" Text
    :> Header "Authorization" ApiToken
    :> Get '[JSON] ConfigResponse


taskStateConfigHandler ::
  Text ->
  Text ->
  Maybe ApiToken ->
  AppM ConfigResponse
taskStateConfigHandler _ _ Nothing = throw missingAuthError
taskStateConfigHandler reqType taskStateName (Just token) = do
  p <- asks pool
  res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.updateTaskStateConfiguration p token reqType taskStateName

  output <- either Err.logError pure res

  return defaultConfigResponse

-- GoalClass Config API and Server
type GoalClassConfig =
  "api"
    :> "v1"
    :> "users"
    :> "current"
    :> "config"
    :> "goalclasses"
    :> Capture "req-type" Text
    :> Capture "goal-class-name" Text
    :> Header "Authorization" ApiToken
    :> Get '[JSON] ConfigResponse


goalClassConfigHandler ::
  Text ->
  Text ->
  Maybe ApiToken ->
  AppM ConfigResponse
goalClassConfigHandler _ _ Nothing = throw missingAuthError
goalClassConfigHandler reqType goalClassName (Just token) = do
  p <- asks pool
  res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.updateGoalClassConfiguration p token reqType goalClassName

  output <- either Err.logError pure res

  return defaultConfigResponse

  
-- MajCat Config API and Server
type MajCatConfig =
  "api"
    :> "v1"
    :> "users"
    :> "current"
    :> "config"
    :> "majcat"
    :> Capture "req-type" Text
    :> Capture "maj-category-name" Text
    :> Header "Authorization" ApiToken
    :> Get '[JSON] ConfigResponse


majCatConfigHandler ::
  Text ->
  Text ->
  Maybe ApiToken ->
  AppM ConfigResponse
majCatConfigHandler _ _ Nothing = throw missingAuthError
majCatConfigHandler reqType majCatName (Just token) = do
  p <- asks pool
  res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.updateMajCatConfiguration p token reqType majCatName

  output <- either Err.logError pure res

  return defaultConfigResponse  


-- SubCat Config API and Server
type SubCatConfig =
  "api"
    :> "v1"
    :> "users"
    :> "current"
    :> "config"
    :> "subcat"
    :> Capture "req-type" Text
    :> Capture "sub-category-name" Text
    :> Capture "maj-category-id" Int64
    :> Header "Authorization" ApiToken
    :> Get '[JSON] ConfigResponse


subCatConfigHandler ::
  Text ->
  Text ->
  Int64 ->
  Maybe ApiToken ->
  AppM ConfigResponse
subCatConfigHandler _ _ _ Nothing = throw missingAuthError
subCatConfigHandler reqType subCatName majCatId (Just token) = do
  p <- asks pool
  res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.updateSubCatConfiguration p token reqType subCatName majCatId

  output <- either Err.logError pure res

  return defaultConfigResponse

-- SubSubCat Config API and Server
type SubSubCatConfig =
  "api"
    :> "v1"
    :> "users"
    :> "current"
    :> "config"
    :> "subsubcat"
    :> Capture "req-type" Text
    :> Capture "sub-sub-category-name" Text
    :> Capture "maj-category-id" Int64
    :> Capture "sub-category-id" Int64    
    :> Header "Authorization" ApiToken
    :> Get '[JSON] ConfigResponse


subSubCatConfigHandler ::
  Text ->
  Text ->
  Int64 ->
  Int64 ->  
  Maybe ApiToken ->
  AppM ConfigResponse
subSubCatConfigHandler _ _ _ _ Nothing = throw missingAuthError
subSubCatConfigHandler reqType subSubCatName majCatId subCatId (Just token) = do
  p <- asks pool
  res <-
    runM
      . embedToMonadIO
      . runError
      $ DbOps.interpretDatabaseIO $
        DbOps.updateSubSubCatConfiguration p token reqType subSubCatName majCatId subCatId

  output <- either Err.logError pure res

  return defaultConfigResponse    

