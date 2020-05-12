module Fedora.Koji
       ( BuildID(..)
       , BuildInfo(..)
       , buildIDInfo
       , kojiBuildTags
       , kojiGetBuildID
       , kojiGetBuildState
       , kojiGetTaskInfo
       , kojiGetTaskChildren
       , kojiGetUserID
       , kojiLatestBuild
       , kojiListTaskIDs
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

readID :: ID a => Struct -> Maybe a
readID st = lookup "id" st >>= fmap mkID . getInt

readID' :: Struct -> Maybe Int
readID' st = lookup "id" st >>= getInt

-- maybeID :: ID a => Maybe a -> Value
-- maybeID = maybe ValueNil ValueInt . fmap getID

getInt :: Value -> Maybe Int
getInt (ValueInt i) = Just i
getInt _ = Nothing

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

data TaskState = TaskFree | TaskOpen | TaskClosed | TaskCanceled | TaskAssigned | TaskFailed
  deriving (Eq, Enum, Show)

openTaskStates :: Value
openTaskStates = ValueArray $ map (ValueInt . fromEnum) [TaskFree, TaskOpen, TaskAssigned]

--taskStateToValue :: TaskState -> Value
--taskStateToValue = ValueInt . fromEnum

readTaskState :: Value -> TaskState
readTaskState (ValueInt i) | i `elem` map fromEnum (enumFrom TaskFree) = toEnum i
readTaskState _ = error "invalid task state"

getTaskState :: Struct -> Maybe TaskState
getTaskState st = readTaskState <$> lookup "state" st

data BuildInfo = BuildInfoID Int | BuildInfoNVR String

buildInfo :: BuildInfo -> Info
buildInfo (BuildInfoID bid) = InfoID (bid)
buildInfo (BuildInfoNVR nvr) = InfoString nvr

buildIDInfo :: BuildID -> BuildInfo
buildIDInfo (BuildId bid) = BuildInfoID bid

kojiGetBuildID :: String -- ^ NVR
           -> IO (Maybe BuildID)
kojiGetBuildID nvr = do
  mbuild <- getBuild $ InfoString nvr
  case mbuild of
    Nothing -> return Nothing
    Just build -> return $ BuildId <$> lookupStruct "id" build

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
  return $ (catMaybes . fmap (lookupStruct "name")) lst

data BuildState = BuildBuilding | BuildComplete | BuildDeleted | BuildFailed | BuildCanceled
  deriving (Eq, Enum, Show)

readBuildState :: Value -> BuildState
readBuildState (ValueInt i) | i `elem` map fromEnum (enumFrom BuildBuilding) = toEnum i
readBuildState _ = error "invalid build state"


kojiGetBuildState :: BuildInfo -> IO (Maybe BuildState)
kojiGetBuildState buildinfo = do
  mbuild <- getBuild (buildInfo buildinfo)
  case mbuild of
    Nothing -> return Nothing
    Just build -> return $ readBuildState <$> lookupStruct "state" build

kojiGetTaskInfo :: TaskID
                -> IO Struct
kojiGetTaskInfo tid = getTaskInfo (getID tid) False
  -- res <- kojiCall "getTaskInfo" [show taskid]
  -- let state = res ^? key "state" % _Integer <&> (toEnum . fromInteger)
  --     arch = res ^? key "arch" % _String
  -- return $ TaskInfo arch state

kojiGetTaskChildren :: TaskID -> Bool -> IO [Struct]
kojiGetTaskChildren tid =
  getTaskChildren (getID tid)

kojiLatestBuild :: String -- ^ tag
                   -> String -- ^ pkg
                   -> IO Value
kojiLatestBuild tag pkg = getLatestBuilds (InfoString tag) Nothing (Just pkg) Nothing
