{-# LANGUAGE OverloadedStrings #-}

module Fedora.Koji
       (
         koji
       , hello
       , getPackageID
       , getTagID
       , getUserID
       , getLatestBuild
       , getTaskInfo
       , getTaskChildren
       , getTaskState
       , getBuildById
       , getBuildByNVR
       , listPackagesSimple
       , listTags
       , listTagsBuild
       , listTagsPackage
       , listTaskIDs
       , checkTagPackage
       , TaskState(..)
       , openstates
       , stateToValue
       , readState
       , TaskID(..)
       , Value(..)
       , displayID
       , getID
       , readID
       , readID'
       , Struct
       , lookupStruct
--       , readStructString
--       , readStructArray
--       , readMethodParams
       , maybeVal
       , getValue
       )
where

import Data.Maybe
--import Control.Monad.Fail (MonadFail)
--import qualified Data.ByteString.Lazy.Char8 as B
import Network.XmlRpc.Client
import Network.XmlRpc.Internals
--import Network.HTTP.Simple
--import Network.HTTP.Client.Conduit
import Control.Monad.Except (runExceptT)

hub :: String
hub = "https://koji.fedoraproject.org/kojihub"

koji :: Remote a =>
        String -- ^ command
     -> a
koji = remote hub

-- xmlrpc :: String -> String -> [Value] -> IO Value
-- xmlrpc url m args = do
--   initreq <- parseRequest url
--   let reqbody = renderCall (MethodCall m args)
--       request = setRequestBody (RequestBodyLBS reqbody) $ setRequestMethod "POST" initreq
--   resp <- httpLBS request
--   let respbody = getResponseBody resp
--   handleError fail $ (parseResponse (B.unpack respbody) >>= handleResponse)
--   where
--     -- | Gets the return value from a method response.
--     --   Throws an exception if the response was a fault.
--     handleResponse :: MonadFail m => MethodResponse -> m Value
--     handleResponse (Return v)       = return v
--     handleResponse (Fault code str) = error ("Error " ++ show code ++ ": " ++ str)

hello :: IO String
hello = koji "hello"

class ID a where
  maybeID :: Maybe Value -> Maybe a
  getID :: a -> Int

displayID :: ID a => a -> String
displayID = show . getID

readID :: ID a => Struct -> Maybe a
readID = maybeID . lookup "id"

readID' :: Struct -> Maybe Int
readID' st =
  case lookup "id" st of
    Just (ValueInt i) -> Just i
    _ -> Nothing

newtype TaskID = TaskId Int
  deriving Show

instance ID TaskID where
  getID (TaskId i) = i
  maybeID (Just (ValueInt i)) = Just (TaskId i)
  maybeID _ = Nothing

newtype TagID = TagId Int
  deriving Show

instance ID TagID where
  getID (TagId i) = i
  maybeID (Just (ValueInt i)) = Just (TagId i)
  maybeID _ = Nothing

newtype UserID = UserId Int
  deriving Show

instance ID UserID where
  getID (UserId i) = i
  maybeID (Just (ValueInt i)) = Just (UserId i)
  maybeID _ = Nothing

newtype BuildID = BuildId Int

instance ID BuildID where
  getID (BuildId i) = i
  maybeID (Just (ValueInt i)) = Just (BuildId i)
  maybeID _ = Nothing

newtype PackageID = PackageId Int

instance ID PackageID where
  getID (PackageId i) = i
  maybeID (Just (ValueInt i)) = Just (PackageId i)
  maybeID _ = Nothing

getPackageID :: String -- ^ package
             -> IO PackageID
getPackageID pkg =
  PackageId <$> koji "getPackageID" pkg


getTagID :: String -- ^ tag
         -> IO TagID
getTagID tag =
  TagId <$> koji "getTagID" tag

type Struct = [(String,Value)]

listPackagesSimple :: String -- ^ package name search prefix
                   -> IO [Struct]
listPackagesSimple =
  koji "listPackagesSimple"

listTags :: IO Value
listTags =
  koji "listTags"

listTagsBuild :: BuildID -- ^ buildid
              -> IO Value
listTagsBuild bid =
  koji "listTags" (getID bid) ()

listTagsPackage :: PackageID
                -> IO Value
listTagsPackage pid =
  koji "listTags" () (getID pid)

checkTagPackage :: TagID
                -> PackageID
                -> IO Bool
checkTagPackage tid pid =
  koji "checkTagPackage" (getID tid) (getID pid)

getLatestBuild :: String -- ^ tag
               -> String -- ^ pkg
               -> IO Value
getLatestBuild tag =
  koji "getLatestBuilds" tag ()

--data TaskInfo = TaskInfo String TaskState

getTaskInfo :: TaskID
            -> Bool -- ^ include request details
            -> IO Struct
getTaskInfo tid =
  koji "getTaskInfo" (getID tid)
  -- res <- kojiCall "getTaskInfo" [show taskid]
  -- let state = res ^? key "state" % _Integer <&> (toEnum . fromInteger)
  --     arch = res ^? key "arch" % _String
  -- return $ TaskInfo arch state

getTaskChildren :: TaskID -> Bool -> IO [Struct]
getTaskChildren tid =
  koji "getTaskChildren" (getID tid)

getBuildById :: BuildID
             -> IO (Maybe Struct)
getBuildById bid =
  maybeStruct <$> koji "getBuild" (getID bid)

maybeStruct :: Value -> Maybe Struct
maybeStruct (ValueStruct st) = Just st
maybeStruct _ = Nothing

getBuildByNVR :: String -- ^ NVR
         -> IO (Maybe Struct)
getBuildByNVR nvr =
  maybeStruct <$> koji "getBuild" nvr

data TaskState = FREE | OPEN | CLOSED | CANCELED | ASSIGNED | FAILED
  deriving (Eq, Enum, Show)

openstates :: Value
openstates = ValueArray $ map (ValueInt . fromEnum) [FREE, OPEN, ASSIGNED]

stateToValue :: TaskState -> Value
stateToValue = ValueInt . fromEnum

readState :: Value -> TaskState
readState (ValueInt i) | i `elem` map fromEnum (enumFrom FREE) = toEnum i
readState _ = error "invalid task state"

listTasks :: Struct -- ^ opts
          -> Struct -- ^ qopts
          -> IO [Struct]
listTasks = koji "listTasks"

listTaskIDs :: Struct -- ^ opts
          -> Struct -- ^ qopts
          -> IO [TaskID]
listTaskIDs opts qopts =
  mapMaybe readID <$> listTasks opts qopts

getUserID :: String -> IO (Maybe UserID)
getUserID name = do
  res <- maybeStruct <$> koji "getUser" name
  return $ readID =<< res

getTaskState :: Struct -> Maybe TaskState
getTaskState st =
  either error readState <$> runExceptT (getField "state" st)
--  readState <$> lookup "state" st

-- readStructString :: String -> Struct -> String
-- readStructString key str =
--   case lookup key str of
--     Just (ValueString s) -> s
--     _ -> error $ "No String for " ++ key

-- readStructArray :: String -> Struct -> Maybe [Value]
-- readStructArray key struct =
--   either error id <$> runExceptT (getField key struct)
-- --    Just (ValueArray s) -> s
-- --    _ -> error $ "No Array for " ++ key

-- readMethodParams :: String -> Maybe [Value]
-- readMethodParams s =
--   either error params <$> runExceptT (parseCall s)
--   where
--     params (MethodCall _m str) = str

lookupStruct :: XmlRpcType a => String -> Struct -> Maybe a
lookupStruct key struct =
  either error id <$> runExceptT (getField key struct)

maybeVal :: String -> Maybe a -> a
maybeVal err = fromMaybe (error err)

getValue :: XmlRpcType a => Value -> Maybe a
getValue = fmap (either error id) . runExceptT . fromValue
