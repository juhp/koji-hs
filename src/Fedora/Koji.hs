module Fedora.Koji
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
       , kojiGetTaskInfo
       , kojiGetTaskChildren
       , kojiGetTaskState
       , kojiGetUserID
       , kojiLatestBuild
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
       , lookupStruct
       )
where

import Data.List
import Data.Maybe
import Network.XmlRpc.Internals

import Fedora.Koji.Internal

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
            maybe True (`isInfixOf` req) msource &&
            maybe True (\ target -> ("<value><string>" ++ target ++ "</string></value>") `isInfixOf` req) mtarget

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

kojiLatestBuild :: String
                -> String -- ^ tag
                -> String -- ^ pkg
                -> IO (Maybe Struct)
kojiLatestBuild hubUrl tag pkg =
  listToMaybe <$> getLatestBuilds hubUrl (InfoString tag) Nothing (Just pkg) Nothing

data KojiBuild
  = KojiBuild
      { kbBuildId :: Int
      , kbPackageId :: Int
      , kbOwnerName :: String
      , kbNvr :: String
      }
  deriving (Show)

lookupInt :: String -> [(String, Value)] -> Maybe Int
lookupInt k values = do
  value <- lookup k values
  getInt value

lookupString :: String -> [(String, Value)] -> Maybe String
lookupString k values = do
  value <- lookup k values
  getString value

kojiListTaggedBuilds :: String -> Bool -> String -> IO [KojiBuild]
kojiListTaggedBuilds hubUrl latest tag =
  mapMaybe readKojiBuild <$> listTagged hubUrl tag Nothing False Nothing latest Nothing Nothing Nothing
  where
    readKojiBuild :: Struct -> Maybe KojiBuild
    readKojiBuild values = do
      buildId <- lookupInt "build_id" values
      packageId <- lookupInt "package_id" values
      owner <- lookupString "owner_name" values
      nvr <- lookupString "nvr" values
      return $ KojiBuild buildId packageId owner nvr

kojiBuildTarget :: String -- ^ hubUrl
                -> String -- ^ target
                -> IO (Maybe (String, String)) -- ^ (build-tag,dest-tag)
kojiBuildTarget hub target =
  readTarget <$> getBuildTarget hub target
  where
  readTarget res = do
    buildtag <- lookupString "build_tag_name" res
    desttag <- lookupString "dest_tag_name" res
    return (buildtag, desttag)
