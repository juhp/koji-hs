module Fedora.Koji
       ( BuildID(..)
       , BuildInfo(..)
       , buildIDInfo
       , kojiBuildTags
       , kojiGetBuildID
       , kojiGetBuildState
       , kojiGetBuildTaskID
       , kojiGetTaskInfo
       , kojiGetTaskChildren
       , kojiGetTaskState
       , kojiGetUserID
       , kojiLatestBuild
       , kojiListTaskIDs
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

import Data.Maybe
import Network.XmlRpc.Internals

import Fedora.Koji.API

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

kojiGetBuildID :: String -- ^ NVR
               -> IO (Maybe BuildID)
kojiGetBuildID nvr =
  ((fmap BuildId . lookupStruct "id") =<<) <$> getBuild (InfoString nvr)

kojiGetBuildTaskID :: String -- ^ NVR
                   -> IO (Maybe TaskID)
kojiGetBuildTaskID nvr =
  ((fmap TaskId . lookupStruct "task_id") =<<) <$> getBuild (InfoString nvr)

kojiListTaskIDs :: Struct -- ^ opts
          -> Struct -- ^ qopts
          -> IO [TaskID]
kojiListTaskIDs opts qopts =
  mapMaybe readID <$> listTasks opts qopts

-- getTagID :: String -- ^ tag
--          -> IO TagID
-- getTagID tag =
--   TagId <$> koji "getTagID" tag

kojiGetUserID :: String -> IO (Maybe UserID)
kojiGetUserID name = do
  res <- getUser (InfoString name) False
  return $ readID =<< res

kojiBuildTags :: BuildInfo -> IO [String]
kojiBuildTags buildinfo = do
  lst <- listTags (Just (buildInfo buildinfo)) Nothing False
  return $ mapMaybe (lookupStruct "name") lst

data BuildState = BuildBuilding | BuildComplete | BuildDeleted | BuildFailed | BuildCanceled
  deriving (Eq, Enum, Show)

readBuildState :: Value -> BuildState
readBuildState (ValueInt i) | i `elem` map fromEnum (enumFrom BuildBuilding) = toEnum i
readBuildState _ = error "invalid build state"

kojiGetBuildState :: BuildInfo -> IO (Maybe BuildState)
kojiGetBuildState buildinfo =
  ((fmap readBuildState . lookupStruct "state") =<<) <$>
  getBuild (buildInfo buildinfo)

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

kojiGetTaskState :: TaskID -> IO (Maybe TaskState)
kojiGetTaskState tid = do
  mti <- getTaskInfo (getID tid) False
  return $ case mti of
             Nothing -> Nothing
             Just ti -> readTaskState <$> lookupStruct "state" ti

kojiGetTaskInfo :: TaskID
                -> IO (Maybe Struct)
kojiGetTaskInfo tid = getTaskInfo (getID tid) True
  -- res <- kojiCall "getTaskInfo" [show taskid]
  -- let state = res ^? key "state" % _Integer <&> (toEnum . fromInteger)
  --     arch = res ^? key "arch" % _String
  -- return $ TaskInfo arch state

kojiGetTaskChildren :: TaskID -> Bool -> IO [Struct]
kojiGetTaskChildren tid =
  getTaskChildren (getID tid)

kojiLatestBuild :: String -- ^ tag
                -> String -- ^ pkg
                -> IO (Maybe Struct)
kojiLatestBuild tag pkg =
  listToMaybe <$> getLatestBuilds (InfoString tag) Nothing (Just pkg) Nothing

data KojiBuild
  = KojiBuild
      { kpBuildId :: Int
      , kpPackageId :: Int
      , kpOwnerName :: String
      , kpNvr :: String
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

readKojiBuild :: Value -> Maybe KojiBuild
readKojiBuild (ValueStruct values) = do
  buildId <- lookupInt "build_id" values
  packageId <- lookupInt "package_id" values
  owner <- lookupString "owner_name" values
  nvr <- lookupString "nvr" values
  return $ KojiBuild buildId packageId owner nvr
readKojiBuild _ = Nothing

kojiListTaggedBuilds :: Bool -> String -> IO [KojiBuild]
kojiListTaggedBuilds latest tag = do
  allValue <- listTagged tag Nothing False Nothing latest Nothing Nothing Nothing
  return $ case allValue of
    ValueArray allArray -> mapMaybe readKojiBuild allArray
    _ -> []
