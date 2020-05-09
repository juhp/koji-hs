{-# LANGUAGE OverloadedStrings #-}

module Fedora.Koji
       ( BuildID(..)
       , getBuildIDByNVR
       , listTaskIDs
       , getPackageID
       , getTagID
       , getUserID
       , buildTags
       , checkTagPackage

       , TaskState(..)
       , TaskID(..)
       , displayID
       , getID
       , readID
       , readID'
       )
where

import Data.Maybe
import Network.XmlRpc.Internals

import Fedora.Koji.Lowlevel

getBuildIDByNVR :: String -- ^ NVR
                -> IO (Maybe BuildID)
getBuildIDByNVR nvr = do
  mbuild <- getBuildByNVR nvr
  case mbuild of
    Nothing -> return Nothing
    Just build -> return $ BuildId <$> lookupStruct "id" build

listTaskIDs :: Struct -- ^ opts
          -> Struct -- ^ qopts
          -> IO [TaskID]
listTaskIDs opts qopts =
  mapMaybe readID <$> listTasks opts qopts

getPackageID :: String -- ^ package
             -> IO PackageID
getPackageID pkg =
  PackageId <$> koji "getPackageID" pkg


getTagID :: String -- ^ tag
         -> IO TagID
getTagID tag =
  TagId <$> koji "getTagID" tag

getUserID :: String -> IO (Maybe UserID)
getUserID name = do
  res <- maybeStruct <$> koji "getUser" name
  return $ readID =<< res

buildTags :: BuildID -> IO (Maybe [String])
buildTags bldid = do
  res <- listTagsBuild bldid
  case res of
    ValueArray lst ->
        return $ (Just . catMaybes . fmap (lookupStruct "name") . mapMaybe maybeStruct) lst
    _ -> error "kojiBuildTags: no result list"

checkTagPackage :: TagID
                -> PackageID
                -> IO Bool
checkTagPackage tid pid =
  koji "checkTagPackage" (getID tid) (getID pid)
