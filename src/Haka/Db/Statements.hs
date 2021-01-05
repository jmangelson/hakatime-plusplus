{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Haka.Db.Statements
  ( insertHeartBeat,
    createAPIToken,
    createBadgeLink,
    getBadgeLinkInfo,
    getTotalActivityTime,
    isUserAvailable,
    updateTokenUsage,
    listApiTokens,
    insertProject,
    getTimeline,
    getUserByToken,
    getUserByRefreshToken,
    deleteRefreshToken,
    deleteAuthToken,
    getUserActivity,
    getProjectStats,
    insertUser,
    getUserByName,
    insertToken,
    createAccessTokens,
    deleteExpiredTokens,
    getClassList,
    insertClass,
    deleteClass,
    getTaskStateList,
    insertTaskState,
    deleteTaskState,
    getGoalClassList,
    insertGoalClass,
    deleteGoalClass,        
    getMajCatList,
    insertMajCat,
    deleteMajCat,
    getSubCatList,
    insertSubCat,
    deleteSubCat,
    getSubSubCatList,
    insertSubSubCat,
    deleteSubSubCat    
  )
where

import Contravariant.Extras.Contrazip (contrazip2, contrazip3, contrazip4, contrazip5)
import qualified Data.ByteString as Bs
import Data.FileEmbed
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Haka.Types
  ( BadgeRow (..),
    EntityType (..),
    HeartbeatPayload (..),
    ProjectStatRow (..),
    RegisteredUser (..),
    StatRow (..),
    StoredApiToken (..),
    TimelineRow (..),
    TokenData (..),
    MajorCategoriesRow (..),
    SubCategoriesRow (..),
    SubSubCategoriesRow (..),    
  )
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Hasql.Statement
import PostgreSQL.Binary.Data (UUID)
import Text.RawString.QQ (r)

updateTokenUsage :: Statement Text ()
updateTokenUsage = Statement query params D.noResult True
  where
    query :: Bs.ByteString
    query =
      [r|
      UPDATE auth_tokens
      SET last_usage = now()::timestamp
      WHERE token = $1
    |]

    params :: E.Params Text
    params = E.param (E.nonNullable E.text)

listApiTokens :: Statement Text [StoredApiToken]
listApiTokens = Statement query params result True
  where
    query :: Bs.ByteString
    query =
      [r| 
      select
        token, last_usage::timestamp
      from
        auth_tokens
      where
        owner = $1 and
        token_expiry is null
      |]

    params :: E.Params Text
    params = E.param (E.nonNullable E.text)

    storedApiToken :: D.Row StoredApiToken
    storedApiToken =
      StoredApiToken
        <$> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nullable) D.timestamptz

    result :: D.Result [StoredApiToken]
    result = D.rowList storedApiToken

createAPIToken :: Statement (Text, Text) ()
createAPIToken = Statement query params D.noResult True
  where
    params :: E.Params (Text, Text)
    params = (fst >$< E.param (E.nonNullable E.text)) <> (snd >$< E.param (E.nonNullable E.text))
    query :: Bs.ByteString
    query = [r| INSERT INTO auth_tokens(owner, token) values($1, $2); |]

createAccessTokens :: Int64 -> Statement TokenData ()
createAccessTokens refreshTokenExpiryHours = Statement query params D.noResult True
  where
    params :: E.Params TokenData
    params =
      (tknOwner >$< E.param (E.nonNullable E.text))
        <> (tknToken >$< E.param (E.nonNullable E.text))
        <> (tknRefreshToken >$< E.param (E.nonNullable E.text))
        <> (const refreshTokenExpiryHours >$< E.param (E.nonNullable E.int8))
    query :: Bs.ByteString
    query =
      [r|
      WITH x AS (
        INSERT INTO auth_tokens(
          owner,
          token,
          token_expiry
        ) values($1, $2, NOW() + interval '30 minutes')
      )
      INSERT INTO refresh_tokens(
        owner,
        refresh_token,
        token_expiry
      ) values($1, $3, NOW() + interval '1' hour * $4);
      |]

deleteExpiredTokens :: Statement TokenData ()
deleteExpiredTokens = Statement query params D.noResult True
  where
    params :: E.Params TokenData
    params =
      (tknOwner >$< E.param (E.nonNullable E.text))
        <> (tknToken >$< E.param (E.nonNullable E.text))
        <> (tknRefreshToken >$< E.param (E.nonNullable E.text))
    query :: Bs.ByteString
    query =
      [r|
      WITH x AS (
        DELETE FROM auth_tokens WHERE owner = $1 AND token_expiry < NOW()
      )

      DELETE FROM refresh_tokens WHERE owner = $1 AND token_expiry < NOW();
      |]

deleteRefreshToken :: Statement Text Int64
deleteRefreshToken = Statement query params D.rowsAffected True
  where
    params :: E.Params Text
    params = E.param (E.nonNullable E.text)
    query :: Bs.ByteString
    query = [r| DELETE FROM refresh_tokens WHERE refresh_token = $1; |]

deleteAuthToken :: Statement Text Int64
deleteAuthToken = Statement query params D.rowsAffected True
  where
    params :: E.Params Text
    params = E.param (E.nonNullable E.text)
    query :: Bs.ByteString
    query = [r| DELETE FROM auth_tokens WHERE token = $1; |]

doubleToUTCTime :: Real a => a -> UTCTime
doubleToUTCTime d = posixSecondsToUTCTime $ realToFrac d

insertToken :: Statement (Text, Text) ()
insertToken =
  Statement
    query
    ( (fst >$< E.param (E.nonNullable E.text))
        <> (snd >$< E.param (E.nonNullable E.text))
    )
    D.noResult
    True
  where
    query :: Bs.ByteString
    query =
      [r|
        INSERT INTO auth_tokens 
        (
          token,
          owner
        )
        VALUES ( $1, $2 );
      |]

getUserByName :: Statement Text (Maybe RegisteredUser)
getUserByName = Statement query (E.param (E.nonNullable E.text)) userDecoder True
  where
    query :: Bs.ByteString
    query = [r| SELECT * FROM users WHERE username = $1;|]
    userDecoder :: D.Result (Maybe RegisteredUser)
    userDecoder = D.rowMaybe user
      where
        user :: D.Row RegisteredUser
        user =
          RegisteredUser
            <$> (D.column . D.nonNullable) D.text
            <*> (D.column . D.nonNullable) D.bytea
            <*> (D.column . D.nonNullable) D.bytea

getTotalActivityTime :: Statement (Text, Int64, Text) (Maybe Int64)
getTotalActivityTime = Statement query params result True
  where
    params :: E.Params (Text, Int64, Text)
    params =
      contrazip3
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.int8))
        (E.param (E.nonNullable E.text))
    query :: Bs.ByteString
    query = $(embedFile "sql/get_total_project_time.sql")
    result :: D.Result (Maybe Int64)
    result = D.rowMaybe $ (D.column . D.nonNullable) D.int8

insertUser :: Statement RegisteredUser ()
insertUser = Statement query params D.noResult True
  where
    params :: E.Params RegisteredUser
    params =
      ( username
          >$< E.param (E.nonNullable E.text)
      )
        <> (hashedPassword >$< E.param (E.nonNullable E.bytea))
        <> (saltUsed >$< E.param (E.nonNullable E.bytea))
    query :: Bs.ByteString
    query =
      [r|
        INSERT INTO users 
        (
          username,
          hashed_password,
          salt_used
        )
        VALUES ( $1, $2, $3 );
      |]

isUserAvailable :: Statement Text (Maybe RegisteredUser)
isUserAvailable = Statement query (E.param (E.nonNullable E.text)) userDecoder True
  where
    query :: Bs.ByteString
    query = [r| SELECT * FROM users WHERE username = $1 |]

    userDecoder :: D.Result (Maybe RegisteredUser)
    userDecoder = D.rowMaybe user
      where
        user :: D.Row RegisteredUser
        user =
          RegisteredUser
            <$> (D.column . D.nonNullable) D.text
            <*> (D.column . D.nonNullable) D.bytea
            <*> (D.column . D.nonNullable) D.bytea

getUserByToken :: Statement (Text, UTCTime) (Maybe Text)
getUserByToken =
  Statement
    query
    ( (fst >$< E.param (E.nonNullable E.text))
        <> (snd >$< E.param (E.nonNullable E.timestamptz))
    )
    (D.rowMaybe ((D.column . D.nonNullable) D.text))
    True
  where
    -- NOTE: On auth tokens the expiry date might not be set.
    -- The tokens created by the CLI do not expire.
    query :: Bs.ByteString
    query =
      [r| 
        SELECT owner FROM auth_tokens 
        WHERE  token = $1 
        AND    COALESCE(
                token_expiry, 
                (NOW() + interval '1 hours')::timestamp without time zone
               ) > $2 ; 
      |]

getUserByRefreshToken :: Statement (Text, UTCTime) (Maybe Text)
getUserByRefreshToken =
  Statement
    query
    ( (fst >$< E.param (E.nonNullable E.text))
        <> (snd >$< E.param (E.nonNullable E.timestamptz))
    )
    (D.rowMaybe ((D.column . D.nonNullable) D.text))
    True
  where
    query :: Bs.ByteString
    query =
      [r| 
      SELECT owner FROM refresh_tokens 
      WHERE 
        refresh_token = $1 AND token_expiry > $2
      ; 
    |]

insertProject :: Statement Text ()
insertProject = Statement query (E.param (E.nonNullable E.text)) D.noResult True
  where
    query :: Bs.ByteString
    query =
      [r|
        INSERT INTO projects (name) VALUES ( $1 ) ON CONFLICT DO NOTHING;
      |]

createBadgeLink :: Statement (Text, Text) UUID
createBadgeLink = Statement query params result True
  where
    params :: E.Params (Text, Text)
    params = (fst >$< E.param (E.nonNullable E.text)) <> (snd >$< E.param (E.nonNullable E.text))
    result :: D.Result UUID
    result = D.singleRow (D.column (D.nonNullable D.uuid))
    query :: Bs.ByteString
    query =
      [r|
        INSERT INTO badges(username, project) values($1, $2)
        ON CONFLICT (username, project) DO UPDATE SET username=EXCLUDED.username
        RETURNING link_id;
     |]

getBadgeLinkInfo :: Statement UUID BadgeRow
getBadgeLinkInfo = Statement query params result True
  where
    params :: E.Params UUID
    params = E.param (E.nonNullable E.uuid)
    result :: D.Result BadgeRow
    result =
      D.singleRow
        ( BadgeRow <$> (D.column . D.nonNullable) D.text
            <*> (D.column . D.nonNullable) D.text
        )
    query :: Bs.ByteString
    query = [r| SELECT username, project FROM badges WHERE link_id = $1; |]

insertHeartBeat :: Statement HeartbeatPayload Int64
insertHeartBeat = Statement query params result True
  where
    result :: D.Result Int64
    result = D.singleRow (D.column (D.nonNullable D.int8))
    query :: Bs.ByteString
    query = $(embedFile "sql/insert_heartbeat.sql")
    params :: E.Params HeartbeatPayload
    params =
      (editor >$< E.param (E.nullable E.text))
        <> (plugin >$< E.param (E.nullable E.text))
        <> (platform >$< E.param (E.nullable E.text))
        <> (machine >$< E.param (E.nullable E.text))
        <> (sender >$< E.param (E.nullable E.text))
        <> (user_agent >$< E.param (E.nonNullable E.text))
        <> (branch >$< E.param (E.nullable E.text))
        <> (category >$< E.param (E.nullable E.text))
        <> (cursorpos >$< E.param (E.nullable E.text))
        <> ( dependencies
               >$< E.param
                 ( E.nullable
                     ( E.array (E.dimension foldl (E.element (E.nonNullable E.text)))
                     )
                 )
           )
        <> (entity >$< E.param (E.nonNullable E.text))
        <> (is_write >$< E.param (E.nullable E.bool))
        <> (language >$< E.param (E.nullable E.text))
        <> (lineno >$< E.param (E.nullable E.int8))
        <> (file_lines >$< E.param (E.nullable E.int8))
        <> (project >$< E.param (E.nullable E.text))
        <> (ty >$< E.param (E.nonNullable entityValue))
        <> ((doubleToUTCTime . time_sent) >$< E.param (E.nonNullable E.timestamptz))
    entityValue :: E.Value EntityType
    entityValue = E.enum entityText
      where
        entityText FileType = "file"
        entityText AppType = "app"
        entityText DomainType = "domain"

getProjectStats :: Statement (Text, Text, UTCTime, UTCTime, Int64) [ProjectStatRow]
getProjectStats = Statement query params result True
  where
    query :: Bs.ByteString
    query = $(embedFile "sql/get_projects_stats.sql")
    params :: E.Params (Text, Text, UTCTime, UTCTime, Int64)
    params =
      contrazip5
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.timestamptz))
        (E.param (E.nonNullable E.timestamptz))
        (E.param (E.nonNullable E.int8))
    projStateRow :: D.Row ProjectStatRow
    projStateRow =
      ProjectStatRow
        <$> (D.column . D.nonNullable) D.timestamptz
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.int8
        <*> (D.column . D.nonNullable) D.numeric
        <*> (D.column . D.nonNullable) D.numeric
    result :: D.Result [ProjectStatRow]
    result = D.rowList projStateRow

getUserActivity :: Statement (Text, UTCTime, UTCTime, Int64) [StatRow]
getUserActivity = Statement query params result True
  where
    query :: Bs.ByteString
    query = $(embedFile "sql/get_user_activity.sql")
    params :: E.Params (Text, UTCTime, UTCTime, Int64)
    params =
      contrazip4
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.timestamptz))
        (E.param (E.nonNullable E.timestamptz))
        (E.param (E.nonNullable E.int8))
    statRow :: D.Row StatRow
    statRow =
      StatRow
        <$> (D.column . D.nonNullable) D.timestamptz
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.int8
        <*> (D.column . D.nonNullable) D.numeric
        <*> (D.column . D.nonNullable) D.numeric
    result :: D.Result [StatRow]
    result = D.rowList statRow

getTimeline :: Statement (Text, UTCTime, UTCTime, Int64) [TimelineRow]
getTimeline = Statement query params result True
  where
    query :: Bs.ByteString
    query = $(embedFile "sql/get_timeline.sql")
    params :: E.Params (Text, UTCTime, UTCTime, Int64)
    params =
      contrazip4
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.timestamptz))
        (E.param (E.nonNullable E.timestamptz))
        (E.param (E.nonNullable E.int8))
    tRow :: D.Row TimelineRow
    tRow =
      TimelineRow
        <$> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.timestamptz
        <*> (D.column . D.nonNullable) D.timestamptz
    result :: D.Result [TimelineRow]
    result = D.rowList tRow


getClassList :: Statement (Text) [Text]
getClassList = Statement query (E.param (E.nonNullable E.text)) result True
  where
    query :: Bs.ByteString
    query =
      [r|
        SELECT classes.name FROM classes WHERE owner = $1;
      |]
    tRow :: D.Row Text
    tRow = (D.column . D.nonNullable) D.text
    result :: D.Result [Text]
    result = D.rowList tRow


insertClass :: Statement (Text, Text) ()
insertClass = Statement query params D.noResult True
  where
    query :: Bs.ByteString
    query =
      [r|
        INSERT INTO classes (name, owner) VALUES ( $1, $2 ) ON CONFLICT (name, owner) DO NOTHING;
      |]
    params :: E.Params (Text, Text)
    params =
      contrazip2
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.text))

deleteClass :: Statement (Text, Text) ()
deleteClass = Statement query params D.noResult True
  where
    query :: Bs.ByteString
    query =
      [r|
        DELETE FROM classes WHERE name = $1 AND owner = $2;
      |]
    params :: E.Params (Text, Text)
    params =
      contrazip2
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.text))

getTaskStateList :: Statement (Text) [Text]
getTaskStateList = Statement query (E.param (E.nonNullable E.text)) result True
  where
    query :: Bs.ByteString
    query =
      [r|
        SELECT task_states.name FROM task_states WHERE owner = $1;
      |]
    tRow :: D.Row Text
    tRow = (D.column . D.nonNullable) D.text
    result :: D.Result [Text]
    result = D.rowList tRow


insertTaskState :: Statement (Text, Text) ()
insertTaskState = Statement query params D.noResult True
  where
    query :: Bs.ByteString
    query =
      [r|
        INSERT INTO task_states (name, owner) VALUES ( $1, $2 ) ON CONFLICT (name, owner) DO NOTHING;
      |]
    params :: E.Params (Text, Text)
    params =
      contrazip2
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.text))

deleteTaskState :: Statement (Text, Text) ()
deleteTaskState = Statement query params D.noResult True
  where
    query :: Bs.ByteString
    query =
      [r|
        DELETE FROM task_states WHERE name = $1 AND owner = $2;
      |]
    params :: E.Params (Text, Text)
    params =
      contrazip2
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.text))        

getGoalClassList :: Statement (Text) [Text]
getGoalClassList = Statement query (E.param (E.nonNullable E.text)) result True
  where
    query :: Bs.ByteString
    query =
      [r|
        SELECT goal_classes.name FROM goal_classes WHERE owner = $1;
      |]
    tRow :: D.Row Text
    tRow = (D.column . D.nonNullable) D.text
    result :: D.Result [Text]
    result = D.rowList tRow


insertGoalClass :: Statement (Text, Text) ()
insertGoalClass = Statement query params D.noResult True
  where
    query :: Bs.ByteString
    query =
      [r|
        INSERT INTO goal_classes (name, owner) VALUES ( $1, $2 ) ON CONFLICT (name, owner) DO NOTHING;
      |]
    params :: E.Params (Text, Text)
    params =
      contrazip2
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.text))

deleteGoalClass :: Statement (Text, Text) ()
deleteGoalClass = Statement query params D.noResult True
  where
    query :: Bs.ByteString
    query =
      [r|
        DELETE FROM goal_classes WHERE name = $1 AND owner = $2;
      |]
    params :: E.Params (Text, Text)
    params =
      contrazip2
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.text))        

getMajCatList :: Statement (Text) [MajorCategoriesRow]
getMajCatList = Statement query (E.param (E.nonNullable E.text)) result True
  where
    query :: Bs.ByteString
    query =
      [r|
        SELECT id, name, owner FROM major_categories WHERE owner = $1;
      |]
    majCatRow :: D.Row MajorCategoriesRow
    majCatRow =
      MajorCategoriesRow
        <$> (D.column . D.nonNullable) D.int8 --D.numeric
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.text        
    result :: D.Result [MajorCategoriesRow]
    result = D.rowList majCatRow    

insertMajCat :: Statement (Text, Text) ()
insertMajCat = Statement query params D.noResult True
  where
    query :: Bs.ByteString
    query =
      [r|
        INSERT INTO major_categories (name, owner) VALUES ( $1, $2 ) ON CONFLICT (name, owner) DO NOTHING;
      |]
    params :: E.Params (Text, Text)
    params =
      contrazip2
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.text))

deleteMajCat :: Statement (Text, Text) ()
deleteMajCat = Statement query params D.noResult True
  where
    query :: Bs.ByteString
    query =
      [r|
        DELETE FROM major_categories WHERE name = $1 AND owner = $2;
      |]
    params :: E.Params (Text, Text)
    params =
      contrazip2
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.text))

getSubCatList :: Statement (Text) [SubCategoriesRow]
getSubCatList = Statement query (E.param (E.nonNullable E.text)) result True
  where
    query :: Bs.ByteString
    query =
      [r|
        SELECT id, name, major_category_id, owner FROM sub_categories WHERE owner = $1;
      |]
    subCatRow :: D.Row SubCategoriesRow
    subCatRow =
      SubCategoriesRow
        <$> (D.column . D.nonNullable) D.int8
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.int8
        <*> (D.column . D.nonNullable) D.text        
    result :: D.Result [SubCategoriesRow]
    result = D.rowList subCatRow    

insertSubCat :: Statement (Text, Int64, Text) ()
insertSubCat = Statement query params D.noResult True
  where
    query :: Bs.ByteString
    query =
      [r|
        INSERT INTO sub_categories (name, major_category_id, owner) VALUES ( $1, $2, $3 ) ON CONFLICT (name, major_category_id, owner) DO NOTHING;
      |]
    params :: E.Params (Text, Int64, Text)
    params =
      contrazip3
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.int8))        
        (E.param (E.nonNullable E.text))        

deleteSubCat :: Statement (Text, Int64, Text) ()
deleteSubCat = Statement query params D.noResult True
  where
    query :: Bs.ByteString
    query =
      [r|
        DELETE FROM sub_categories WHERE name = $1 AND major_category_id = $2 and owner = $3;
      |]
    params :: E.Params (Text, Int64, Text)
    params =
      contrazip3
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.int8))        
        (E.param (E.nonNullable E.text))


getSubSubCatList :: Statement (Text) [SubSubCategoriesRow]
getSubSubCatList = Statement query (E.param (E.nonNullable E.text)) result True
  where
    query :: Bs.ByteString
    query =
      [r|
        SELECT id, name, major_category_id, sub_category_id, owner FROM sub_sub_categories WHERE owner = $1;
      |]
    subCatRow :: D.Row SubSubCategoriesRow
    subCatRow =
      SubSubCategoriesRow
        <$> (D.column . D.nonNullable) D.int8
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.int8
        <*> (D.column . D.nonNullable) D.int8        
        <*> (D.column . D.nonNullable) D.text        
    result :: D.Result [SubSubCategoriesRow]
    result = D.rowList subCatRow    

insertSubSubCat :: Statement (Text, Int64, Int64, Text) ()
insertSubSubCat = Statement query params D.noResult True
  where
    query :: Bs.ByteString
    query =
      [r|
        INSERT INTO sub_sub_categories (name, major_category_id, sub_category_id, owner) VALUES ( $1, $2, $3, $4 ) ON CONFLICT (name, major_category_id, sub_category_id, owner) DO NOTHING;
      |]
    params :: E.Params (Text, Int64, Int64, Text)
    params =
      contrazip4
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.int8))
        (E.param (E.nonNullable E.int8))                
        (E.param (E.nonNullable E.text))        

deleteSubSubCat :: Statement (Text, Int64, Int64, Text) ()
deleteSubSubCat = Statement query params D.noResult True
  where
    query :: Bs.ByteString
    query =
      [r|
        DELETE FROM sub_sub_categories WHERE name = $1 AND major_category_id = $2 AND sub_category_id = $3 and owner = $4;
      |]
    params :: E.Params (Text, Int64, Int64, Text)
    params =
      contrazip4
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.int8))
        (E.param (E.nonNullable E.int8))                
        (E.param (E.nonNullable E.text))           
