module Haka.Db.Sessions
  ( getUser,
    getUserByRefreshToken,
    createBadgeLink,
    getTotalActivityTime,
    updateTokenUsage,
    deleteToken,
    createAPIToken,
    listApiTokens,
    saveHeartbeats,
    getTotalStats,
    getTimeline,
    getProjectStats,
    insertToken,
    insertUser,
    validateUser,
    deleteTokens,
    getBadgeLinkInfo,
    createAccessTokens,
    updateClassConfig,
    getClassList,
    updateTaskStateConfig,
    getTaskStateList,
    updateGoalClassConfig,
    getGoalClassList,        
    updateMajCatConfig,
    getMajCatList,
    updateSubCatConfig,
    getSubCatList,
    updateSubSubCatConfig,
    getSubSubCatList,            
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Crypto.Error as CErr
import Data.Int (Int64)
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Haka.Db.Statements as Statements
import Haka.Types
  ( ApiToken (..),
    BadgeRow (..),
    HeartbeatPayload (..),
    ProjectStatRow (..),
    RegisteredUser (..),
    StatRow (..),
    StoredApiToken,
    TimelineRow (..),
    TokenData (..),
    MajorCategoriesRow (..),
    SubCategoriesRow (..),
    SubSubCategoriesRow (..),  
  )
import qualified Haka.Utils as Utils
import Hasql.Session (Session, statement)
import qualified Hasql.Transaction as Transaction
import Hasql.Transaction.Sessions (IsolationLevel (..), Mode (..), transaction)
import PostgreSQL.Binary.Data (UUID)
import Debug.Trace

updateTokenUsage :: Text -> Session ()
updateTokenUsage tkn = statement tkn Statements.updateTokenUsage

listApiTokens :: Text -> Session [StoredApiToken]
listApiTokens usr = statement usr Statements.listApiTokens

getUser :: ApiToken -> Session (Maybe Text)
getUser (ApiToken token) = do
  now <- liftIO getCurrentTime
  statement (token, now) Statements.getUserByToken

getUserByRefreshToken :: Text -> Session (Maybe Text)
getUserByRefreshToken token = do
  now <- liftIO getCurrentTime
  statement (token, now) Statements.getUserByRefreshToken

deleteToken :: ApiToken -> Session ()
deleteToken (ApiToken token) = do
  _ <- statement token Statements.deleteAuthToken
  pure ()

deleteTokens :: ApiToken -> Text -> Session Int64
deleteTokens (ApiToken token) refreshToken = do
  r1 <-
    transaction
      Serializable
      Write
      (Transaction.statement token Statements.deleteAuthToken)
  r2 <-
    transaction
      Serializable
      Write
      (Transaction.statement refreshToken Statements.deleteRefreshToken)
  pure (r1 + r2)

saveHeartbeats :: [HeartbeatPayload] -> Session [Int64]
saveHeartbeats payloadData = do
  -- Create the projects first so they can be referenced from the heartbeats.
  mapM_ (`statement` Statements.insertProject) uniqueProjects
  -- Insert the heartbeats.
  mapM (`statement` Statements.insertHeartBeat) payloadData
  where
    uniqueProjects = nub $ mapMaybe project payloadData

getTotalActivityTime :: Text -> Int64 -> Text -> Session (Maybe Int64)
getTotalActivityTime user days proj = statement (user, days, proj) Statements.getTotalActivityTime

createBadgeLink :: Text -> Text -> Session UUID
createBadgeLink user proj = statement (user, proj) Statements.createBadgeLink

getBadgeLinkInfo :: UUID -> Session BadgeRow
getBadgeLinkInfo badgeId = statement badgeId Statements.getBadgeLinkInfo

-- | TODO: Impose a max limit
-- | Retrieve computed statistics for a given range.
getTotalStats :: Text -> (UTCTime, UTCTime) -> Int64 -> Session [StatRow]
getTotalStats user (startDate, endDate) cutOffLimit =
  statement (user, startDate, endDate, cutOffLimit) Statements.getUserActivity

getTimeline :: Text -> (UTCTime, UTCTime) -> Int64 -> Session [TimelineRow]
getTimeline user (startDate, endDate) cutOffLimit =
  statement (user, startDate, endDate, cutOffLimit) Statements.getTimeline

getProjectStats :: Text -> Text -> (UTCTime, UTCTime) -> Int64 -> Session [ProjectStatRow]
getProjectStats user proj (startDate, endDate) cutOffLimit =
  statement (user, proj, startDate, endDate, cutOffLimit) Statements.getProjectStats

insertUser :: RegisteredUser -> Session Bool
insertUser aUser = do
  r <- statement (username aUser) Statements.isUserAvailable
  case r of
    Just _ -> pure False
    Nothing -> do
      statement aUser Statements.insertUser
      pure True

validateUser ::
  (RegisteredUser -> Text -> Text -> Either CErr.CryptoError Bool) ->
  Text ->
  Text ->
  Session Bool
validateUser validate name pass = do
  res <- statement name Statements.getUserByName
  case res of
    Nothing -> pure False
    Just savedUser ->
      case validate savedUser name pass of
        Left e -> do
          liftIO $
            putStrLn $
              "failed to validate user password (" <> show name <> "): " <> show e
          pure False
        Right v -> pure v

-- | Insert a newly generated token for the given user.
-- | The token must be hashed and the salt should be saved.
insertToken :: Text -> Text -> Session ()
insertToken token name = statement (token, name) Statements.insertToken

createAccessTokens :: Int64 -> TokenData -> Session ()
createAccessTokens refreshTokenExpiryHours tknData = do
  transaction
    Serializable
    Write
    (Transaction.statement tknData (Statements.createAccessTokens refreshTokenExpiryHours))
  transaction
    Serializable
    Write
    (Transaction.statement tknData Statements.deleteExpiredTokens)

createAPIToken :: Text -> Session Text
createAPIToken usr = do
  newToken <- liftIO Utils.randomToken
  statement (usr, Utils.toBase64 newToken) Statements.createAPIToken
  pure newToken

updateClassConfig :: Text -> Text -> Text -> Session ()
updateClassConfig user req_type class_name = do
  if trace("In updateClassConfig, with req_type=" ++ show req_type ++ ", class=" ++ show class_name ++ ", user= " ++ show user) (req_type == "add")
    then do
         trace ("In updateClassConfig, calling Statements.insertClass with class=" ++ show class_name ++ ", user= " ++ show user) (statement (class_name, user) Statements.insertClass)
    else if req_type == "remove"
           then statement (class_name, user) Statements.deleteClass
           else statement ("none", "none") Statements.deleteClass

getClassList :: Text -> Session [Text]
getClassList user = do
  statement user Statements.getClassList

updateTaskStateConfig :: Text -> Text -> Text -> Session ()
updateTaskStateConfig user req_type class_name = do
  if trace("In updateTaskStateConfig, with req_type=" ++ show req_type ++ ", class=" ++ show class_name ++ ", user= " ++ show user) (req_type == "add")
    then do
         trace ("In updateTaskStateConfig, calling Statements.insertTaskState with class=" ++ show class_name ++ ", user= " ++ show user) (statement (class_name, user) Statements.insertTaskState)
    else if req_type == "remove"
           then statement (class_name, user) Statements.deleteTaskState
           else statement ("none", "none") Statements.deleteTaskState

getTaskStateList :: Text -> Session [Text]
getTaskStateList user = do
  statement user Statements.getTaskStateList

updateGoalClassConfig :: Text -> Text -> Text -> Session ()
updateGoalClassConfig user req_type class_name = do
  if trace("In updateGoalClassConfig, with req_type=" ++ show req_type ++ ", class=" ++ show class_name ++ ", user= " ++ show user) (req_type == "add")
    then do
         trace ("In updateGoalClassConfig, calling Statements.insertGoalClass with class=" ++ show class_name ++ ", user= " ++ show user) (statement (class_name, user) Statements.insertGoalClass)
    else if req_type == "remove"
           then statement (class_name, user) Statements.deleteGoalClass
           else statement ("none", "none") Statements.deleteGoalClass

getGoalClassList :: Text -> Session [Text]
getGoalClassList user = do
  statement user Statements.getGoalClassList

  
updateMajCatConfig :: Text -> Text -> Text -> Session ()
updateMajCatConfig user req_type class_name = do
  if trace("In updateMajCatConfig, with req_type=" ++ show req_type ++ ", class=" ++ show class_name ++ ", user= " ++ show user) (req_type == "add")
    then do
         trace ("In updateMajCatConfig, calling Statements.insertMajCat with class=" ++ show class_name ++ ", user= " ++ show user) (statement (class_name, user) Statements.insertMajCat)
    else if req_type == "remove"
           then statement (class_name, user) Statements.deleteMajCat
           else statement ("none", "none") Statements.deleteMajCat

getMajCatList :: Text -> Session [MajorCategoriesRow]
getMajCatList user = do
   trace("In getMajCatList, calling Statements.getMajCatList, with user=" ++ show user) (statement user Statements.getMajCatList)

updateSubCatConfig :: Text -> Text -> Text -> Int64 -> Session ()
updateSubCatConfig user req_type class_name maj_cat_id = do
  if trace("In updateSubCatConfig, with req_type=" ++ show req_type ++ ", class=" ++ show class_name ++ ", user= " ++ show user) (req_type == "add")
    then do
         trace ("In updateSubCatConfig, calling Statements.insertSubCat with class=" ++ show class_name ++ ", user= " ++ show user) (statement (class_name, maj_cat_id, user) Statements.insertSubCat)
    else if req_type == "remove"
           then statement (class_name, maj_cat_id, user) Statements.deleteSubCat
           else statement ("none", 0, "none") Statements.deleteSubCat

getSubCatList :: Text -> Session [SubCategoriesRow]
getSubCatList user = do
   trace("In getSubCatList, calling Statements.getSubCatList, with user=" ++ show user) (statement user Statements.getSubCatList)

updateSubSubCatConfig :: Text -> Text -> Text -> Int64 -> Int64 -> Session ()
updateSubSubCatConfig user req_type class_name maj_cat_id sub_cat_id = do
  if trace("In updateSubsubCatConfig, with req_type=" ++ show req_type ++ ", class=" ++ show class_name ++ ", user= " ++ show user) (req_type == "add")
    then do
         trace ("In updateSubSubCatConfig, calling Statements.insertSubsubCat with class=" ++ show class_name ++ ", user= " ++ show user) (statement (class_name, maj_cat_id, sub_cat_id, user) Statements.insertSubSubCat)
    else if req_type == "remove"
           then statement (class_name, maj_cat_id, sub_cat_id, user) Statements.deleteSubSubCat
           else statement ("none", 0, 0, "none") Statements.deleteSubSubCat

getSubSubCatList :: Text -> Session [SubSubCategoriesRow]
getSubSubCatList user = do
   trace("In getSubSubCatList, calling Statements.getSubSubCatList, with user=" ++ show user) (statement user Statements.getSubSubCatList)
