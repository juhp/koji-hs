module Distribution.Koji
       ( BuildID(..)
       , BuildInfo(..)
       , buildIDInfo
       , fedoraKojiHub
       , centosKojiHub
       , kojiBuildTags
       , kojiBuildTarget
       , kojiGetBuildID
       , kojiGetBuildState
       , kojiGetBuildTaskID
       , kojiGetCurrentRepo
       , kojiGetRepo
       , kojiGetTaskInfo
       , kojiGetTaskChildren
       , kojiGetTaskState
       , kojiGetUserID
       , kojiLatestBuild
       , kojiLatestBuildRepo
       , kojiListSideTags
       , kojiListTaskIDs
       , kojiUserBuildTasks
       , KojiBuild(..)
       , kojiListTaggedBuilds
       , PackageID(..)
       , TagID(..)
       , TaskID(..)
       , UserID(..)
       , displayID
       , getID
       , readID
       , readID'
       , TaskState(..)
       , getTaskState
       , openTaskStates
       , openTaskValues
       , readTaskState
       , BuildState(..)
       , readBuildState
       , Struct
       , lookupStruct
       , Value (..)
       , getInt
       , getString
       , maybeVal
       , RepoState(..)
       , readRepoState
       )
where

import qualified Data.List as L
import Data.Maybe
import Network.XmlRpc.Internals

import Distribution.Koji.Internal

class ID a where
  getID :: a -> Int
  mkID :: Int -> a

displayID :: ID a => a -> String
displayID = show . getID

-- FIXME rename to structID ?
readID :: ID a => Struct -> Maybe a
readID st = lookup "id" st >>= fmap mkID . getInt

readID' :: Struct -> Maybe Int
readID' st = lookup "id" st >>= getInt

-- FIXME rename to valueInt ?
getInt :: Value -> Maybe Int
getInt (ValueInt i) = Just i
getInt _ = Nothing

getString :: Value -> Maybe String
getString (ValueString i) = Just i
getString _ = Nothing

newtype TaskID = TaskId Int
  deriving Show

instance ID TaskID where
  getID (TaskId i) = i
  mkID = TaskId

newtype TagID = TagId Int
  deriving Show

instance ID TagID where
  getID (TagId i) = i
  mkID = TagId

newtype UserID = UserId Int
  deriving Show

instance ID UserID where
  getID (UserId i) = i
  mkID = UserId

newtype BuildID = BuildId Int

instance ID BuildID where
  getID (BuildId i) = i
  mkID = BuildId

newtype PackageID = PackageId Int

instance ID PackageID where
  getID (PackageId i) = i
  mkID = PackageId

newtype BuildrootID = BuildrootId Int

instance ID BuildrootID where
  getID (BuildrootId i) = i
  mkID = BuildrootId

data BuildInfo = BuildInfoID Int | BuildInfoNVR String

buildInfo :: BuildInfo -> Info
buildInfo (BuildInfoID bid) = InfoID bid
buildInfo (BuildInfoNVR nvr) = InfoString nvr

buildIDInfo :: BuildID -> BuildInfo
buildIDInfo (BuildId bid) = BuildInfoID bid

fedoraKojiHub :: String
fedoraKojiHub = "https://koji.fedoraproject.org/kojihub"

centosKojiHub :: String
centosKojiHub = "https://koji.mbox.centos.org/kojihub"

kojiGetBuildID :: String -- ^ hub url
               -> String -- ^ NVR
               -> IO (Maybe BuildID)
kojiGetBuildID hubUrl nvr =
  ((fmap BuildId . lookupStruct "id") =<<) <$> getBuild hubUrl (InfoString nvr)

kojiGetBuildTaskID :: String -- ^ hub url
                   -> String -- ^ NVR
                   -> IO (Maybe TaskID)
kojiGetBuildTaskID hubUrl nvr =
  ((fmap TaskId . lookupStruct "task_id") =<<) <$> getBuild hubUrl (InfoString nvr)

kojiListTaskIDs :: String
                -> Struct -- ^ opts
                -> Struct -- ^ qopts
                -> IO [TaskID]
kojiListTaskIDs hubUrl opts qopts =
  mapMaybe readID <$> listTasks hubUrl opts qopts

kojiUserBuildTasks :: String -> UserID -> Maybe String -> Maybe String -> IO [TaskID]
kojiUserBuildTasks hubUrl userid msource mtarget = do
  tasks <- listTasks hubUrl [("owner",ValueInt (getID userid)),("method",ValueString "build"),("state",openTaskValues)] []
  return $ map TaskId . mapMaybe (lookupStruct "id") $ filter isTheBuild tasks
  where
    isTheBuild :: Struct -> Bool
    isTheBuild st =
      let mreq = lookupStruct "request" st in
        case mreq of
          Nothing -> False
          Just req ->
            maybe True (`L.isInfixOf` req) msource &&
            maybe True (\ target -> ("<value><string>" ++ target ++ "</string></value>") `L.isInfixOf` req) mtarget

-- getTagID :: String -- ^ tag
--          -> IO TagID
-- getTagID tag =
--   TagId <$> koji "getTagID" tag

kojiGetUserID :: String -> String -> IO (Maybe UserID)
kojiGetUserID hubUrl name = do
  res <- getUser hubUrl (InfoString name) False
  return $ readID =<< res

kojiBuildTags :: String -> BuildInfo -> IO [String]
kojiBuildTags hubUrl buildinfo = do
  lst <- listTags hubUrl (Just (buildInfo buildinfo)) Nothing False
  return $ mapMaybe (lookupStruct "name") lst

data BuildState = BuildBuilding | BuildComplete | BuildDeleted | BuildFailed | BuildCanceled
  deriving (Eq, Enum, Show)

readBuildState :: Value -> BuildState
readBuildState (ValueInt i) | i `elem` map fromEnum (enumFrom BuildBuilding) = toEnum i
readBuildState _ = error "invalid build state"

kojiGetBuildState :: String -> BuildInfo -> IO (Maybe BuildState)
kojiGetBuildState hubUrl buildinfo =
  ((fmap readBuildState . lookupStruct "state") =<<) <$>
  getBuild hubUrl (buildInfo buildinfo)

data TaskState = TaskFree | TaskOpen | TaskClosed | TaskCanceled | TaskAssigned | TaskFailed
  deriving (Eq, Enum, Show)

openTaskStates :: [TaskState]
openTaskStates = [TaskFree, TaskOpen, TaskAssigned]

openTaskValues :: Value
openTaskValues = ValueArray $ map taskStateToValue openTaskStates
  where
    taskStateToValue :: TaskState -> Value
    taskStateToValue = ValueInt . fromEnum

readTaskState :: Value -> TaskState
readTaskState (ValueInt i) | i `elem` map fromEnum (enumFrom TaskFree) = toEnum i
readTaskState _ = error "invalid task state"

getTaskState :: Struct -> Maybe TaskState
getTaskState st = readTaskState <$> lookup "state" st

kojiGetTaskState :: String -> TaskID -> IO (Maybe TaskState)
kojiGetTaskState hubUrl tid = do
  mti <- getTaskInfo hubUrl (getID tid) False
  return $ case mti of
             Nothing -> Nothing
             Just ti -> readTaskState <$> lookupStruct "state" ti

kojiGetTaskInfo :: String
                -> TaskID
                -> IO (Maybe Struct)
kojiGetTaskInfo hubUrl tid = getTaskInfo hubUrl (getID tid) True
  -- res <- kojiCall "getTaskInfo" [show taskid]
  -- let state = res ^? key "state" % _Integer <&> (toEnum . fromInteger)
  --     arch = res ^? key "arch" % _String
  -- return $ TaskInfo arch state

kojiGetTaskChildren :: String -> TaskID -> Bool -> IO [Struct]
kojiGetTaskChildren hubUrl tid =
  getTaskChildren hubUrl (getID tid)

kojiLatestBuild :: String -- ^ hub
                -> String -- ^ tag
                -> String -- ^ pkg
                -> IO (Maybe Struct)
kojiLatestBuild hubUrl tag pkg =
  listToMaybe <$> getLatestBuilds hubUrl (InfoString tag) Nothing (Just pkg) Nothing

kojiLatestBuildRepo :: String -- ^ hub
                    -> String -- ^ tag
                    -> Int    -- ^ event
                    -> String -- ^ pkg
                    -> IO (Maybe Struct)
kojiLatestBuildRepo hubUrl tag event pkg =
  listToMaybe <$> getLatestBuilds hubUrl (InfoString tag) (Just event) (Just pkg) Nothing

data KojiBuild
  = KojiBuild
      { kbBuildId :: Int
      , kbPackageId :: Int
      , kbOwnerName :: String
      , kbNvr :: String
      }
  deriving (Show)

kojiListTaggedBuilds :: String -> Bool -> String -> IO [KojiBuild]
kojiListTaggedBuilds hubUrl latest tag =
  mapMaybe readKojiBuild <$> listTagged hubUrl tag Nothing False Nothing latest Nothing Nothing Nothing
  where
    readKojiBuild :: Struct -> Maybe KojiBuild
    readKojiBuild values = do
      buildId <- lookupStruct "build_id" values
      packageId <- lookupStruct "package_id" values
      owner <- lookupStruct "owner_name" values
      nvr <- lookupStruct "nvr" values
      return $ KojiBuild buildId packageId owner nvr

kojiBuildTarget :: String -- ^ hubUrl
                -> String -- ^ target
                -> IO (Maybe (String, String)) -- ^ (build-tag,dest-tag)
kojiBuildTarget hub target = do
  mres <- maybeStruct <$> getBuildTarget hub target
  case mres of
    Nothing -> return Nothing
    Just res -> return $ readTarget res
  where
  readTarget res = do
    buildtag <- lookupStruct "build_tag_name" res
    desttag <- lookupStruct "dest_tag_name" res
    return (buildtag, desttag)

-- | List sidetags (preferably for user and/or basetag)
kojiListSideTags :: String -- ^ hubUrl
                 -> Maybe String -- ^ basetag
                 -> Maybe String -- ^ user
                 -> IO [String] -- ^ list of sidetags
kojiListSideTags hub mbasetag muser =
  mapMaybe (lookupStruct "name") . structArray <$> listSideTags hub (InfoString <$> mbasetag) (InfoString <$> muser)

data RepoState = RepoInit | RepoReady | RepoExpired | RepoDeleted | RepoProblem
  deriving (Eq, Enum, Show)

readRepoState :: Value -> RepoState
readRepoState (ValueInt i) | i `elem` map fromEnum (enumFrom RepoInit) = toEnum i
readRepoState _ = error "invalid repo state"

-- getRepoState :: Struct -> Maybe RepoState
-- getRepoState st = readRepoState <$> lookup "state" st

kojiGetRepo :: String -> String -> Maybe RepoState -> Maybe Int
            -> IO (Maybe Struct)
kojiGetRepo hub tag mstate mevent =
  maybeStruct <$> getRepo hub tag (fromEnum <$> mstate) mevent False

kojiGetCurrentRepo :: String -> String -> IO (Maybe Struct)
kojiGetCurrentRepo hub tag =
  maybeStruct <$> getRepo hub tag Nothing Nothing False
