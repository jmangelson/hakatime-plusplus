module Haka.Api
  ( api,
    server,
    heartbeatApi,
  )
where

import qualified Haka.Authentication as Auth
import qualified Haka.Badges as Badges
import qualified Haka.Heartbeats as Heartbeats
import qualified Haka.Projects as Projects
import qualified Haka.Stats as Stats
import qualified Haka.Config as Config
import Haka.Types (AppM, ServerSettings (..))
import qualified Haka.Users as Users
import Servant

type Static = Raw

heartbeatApi :: Proxy Heartbeats.API
heartbeatApi = Proxy

-- Combined API type for each sub-api available.
type HakaAPI =
  Heartbeats.API
    :<|> Stats.API
    :<|> Projects.API
    :<|> Auth.API
    :<|> Badges.API
    :<|> Users.API
    :<|> Config.API    
    :<|> Static

api :: Proxy HakaAPI
api = Proxy

-- The API handlers should be presented in the same order as in the API type.
server :: ServerSettings -> ServerT HakaAPI AppM
server settings =
  Heartbeats.server
    :<|> Stats.server
    :<|> Projects.server
    :<|> Auth.server (hakaEnableRegistration settings)
    :<|> Badges.server
    :<|> Users.server
    :<|> Config.server    
    :<|> serveDirectoryFileServer (hakaDashboardPath settings)
