{-# LANGUAGE OverloadedStrings #-}

module Koji
       ( kojiCall,
         hello,
         listTags,
         getPackageID,
         getTagID,
         listPackagesSimple,
         checkTagPackage,
         getLatestBuild
       ) where

import Data.Text (Text)
import Lens.Micro
import Lens.Micro.Aeson
import SimpleCmd

--hub :: String
--hub = "https://koji.fedoraproject.org/kojihub"
--hub = "https://brewhub.engineering.redhat.com/brewhub"

kojiCall :: String -> [String] -> IO String
kojiCall c args = do
  cmd "koji" ("call":"--json-output":c:args)

listTags :: IO [Text]
listTags = do
  res <- kojiCall "listTags" []
  return $ res ^.. values . key "name" . _String

hello :: IO Text
hello = do
  res <- kojiCall "hello" []
  return $ res ^. _String

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
  return $ res ^.. values . key "package_name" . _String

checkTagPackage :: Int -> Int -> IO Bool
checkTagPackage tagid pkgid = do
  res <- kojiCall "checkTagPackage" [show tagid,show pkgid]
  return (res ^? _Bool == Just True)

getLatestBuild :: String -> String -> IO (Maybe Text)
getLatestBuild tag pkg = do
  res <- kojiCall "getLatestBuilds" [tag, "None", pkg]
  return $ res ^? values . key "nvr" . _String
