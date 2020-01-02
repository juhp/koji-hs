{-# LANGUAGE OverloadedStrings #-}

module Koji
       ( kojiCall,
         hello,
         listTags,
         listTagsBuild,
         listTagsPackage,
         getPackageID,
         getTagID,
         listPackagesSimple,
         checkTagPackage,
         getLatestBuild,
         getTaskInfo,
         TaskState(..),
         getBuild
       ) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text (Text)
import Optics.Core
import Data.Aeson.Optics
import System.Process.Typed

kojiCall :: String -> [String] -> IO Value
kojiCall c args = do
  (out,err) <- readProcess_ (proc "koji" ("call":"--json-output":c:args))
  case decode out of
    Nothing -> error (B.unpack err) -- $ "Failure for: koji call " ++ unwords (c:args)
    Just v -> return v

listTags :: IO [Text]
listTags = do
  res <- kojiCall "listTags" []
  return $ res ^.. values % key "name" % _String

listTagsBuild :: String -> IO [Text]
listTagsBuild bld = do
  res <- kojiCall "listTags" [bld]
  return $ res ^.. values % key "name" % _String

listTagsPackage :: String -> IO [Text]
listTagsPackage pkg = do
  res <- kojiCall "listTags" ["None", pkg]
  return $ res ^.. values % key "name" % _String

hello :: IO (Maybe Text)
hello = do
  res <- kojiCall "hello" []
  return $ res ^? _String

getPackageID :: String -> IO (Maybe Integer)
getPackageID pkg = do
  res <- kojiCall "getPackageID" [pkg]
  return $ res ^? _Integer

getTagID :: String -> IO (Maybe Integer)
getTagID tag = do
  res <- kojiCall "getTagID" [tag]
  return $ res ^? _Integer

listPackagesSimple :: String -> IO [Text]
listPackagesSimple prefix = do
  res <- kojiCall "listPackagesSimple" [prefix]
  return $ res ^.. values % key "package_name" % _String

checkTagPackage :: Int -> Int -> IO Bool
checkTagPackage tagid pkgid = do
  res <- kojiCall "checkTagPackage" [show tagid,show pkgid]
  return (res ^? _Bool == Just True)

getLatestBuild :: String -> String -> IO (Maybe Text)
getLatestBuild tag pkg = do
  res <- kojiCall "getLatestBuilds" [tag, "None", pkg]
  return $ res ^? key "nvr" % _String

data TaskState = FREE | OPEN | CLOSED | CANCELED | ASSIGNED | FAILED
  deriving (Eq, Enum)

--data TaskInfo = TaskInfo String TaskState

getTaskInfo :: Int -> IO Value --(Maybe TaskState)
getTaskInfo taskid = do
  {-res <--} kojiCall "getTaskInfo" [show taskid]
  -- let state = res ^? key "state" % _Integer <&> (toEnum . fromInteger)
  --     arch = res ^? key "arch" % _String
  -- return $ TaskInfo arch state

getBuild :: Int -> IO Value
getBuild buildid = do
  {-res <--} kojiCall "getBuild" [show buildid]
--  return $ res ^? key "state" % _Integer <&> (toEnum . fromInteger)


