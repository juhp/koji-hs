{-# LANGUAGE OverloadedStrings #-}

module Koji
       (
         koji
       , hello
       , getPackageID
       , getTagID
       , listPackagesSimple
       , listTags
       , listTagsBuild
       , listTagsPackage
       , checkTagPackage
       , getLatestBuild
       , getTaskInfo
       , getBuildById
       , getBuildByNVR
       , listTasks
       )
where

--import Control.Monad.Fail (MonadFail)
--import qualified Data.ByteString.Lazy.Char8 as B
import Network.XmlRpc.Client
import Network.XmlRpc.Internals
--import Network.HTTP.Simple
--import Network.HTTP.Client.Conduit

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

getPackageID :: String -- ^ package
             -> IO Int -- ^ pkgid
getPackageID =
  koji "getPackageID"

getTagID :: String -- ^ tag
         -> IO Int -- ^ tagid
getTagID =
  koji "getTagID"

type Struct = [(String,Value)]

listPackagesSimple :: String -- ^ package name search prefix
                   -> IO [Struct]
listPackagesSimple =
  koji "listPackagesSimple"

listTags :: IO Value
listTags =
  koji "listTags"

listTagsBuild :: Int -- ^ buildid
              -> IO Value
listTagsBuild buildid =
  koji "listTags" buildid ()

listTagsPackage :: Int -- ^ pkgid
                -> IO Value
listTagsPackage =
  koji "listTags" ()

checkTagPackage :: Int -- ^ tagid
                -> Int -- ^ pkgid
                -> IO Bool
checkTagPackage =
  koji "checkTagPackage"

getLatestBuild :: String -- ^ tag
               -> String -- ^ pkg
               -> IO Value
getLatestBuild tag =
  koji "getLatestBuilds" tag ()

-- data TaskState = FREE | OPEN | CLOSED | CANCELED | ASSIGNED | FAILED
--   deriving (Eq, Enum)

--data TaskInfo = TaskInfo String TaskState

getTaskInfo :: Int -- ^ taskid
            -> IO Struct
getTaskInfo =
  koji "getTaskInfo"
  -- res <- kojiCall "getTaskInfo" [show taskid]
  -- let state = res ^? key "state" % _Integer <&> (toEnum . fromInteger)
  --     arch = res ^? key "arch" % _String
  -- return $ TaskInfo arch state

getBuildById :: Int -- ^ buildid
         -> IO (Maybe Struct)
getBuildById bid =
  maybeStruct <$> koji "getBuild" bid

maybeStruct :: Value -> Maybe Struct
maybeStruct (ValueStruct st) = Just st
maybeStruct _ = Nothing

getBuildByNVR :: String -- ^ NVR
         -> IO (Maybe Struct)
getBuildByNVR nvr =
  maybeStruct <$> koji "getBuild" nvr

-- data TaskState = FREE | OPEN | CLOSED | CANCELED | ASSIGNED | FAILED
--   deriving (Eq, Enum)

-- openstates :: Value
-- openstates = ValueArray $ map (ValueInt . fromEnum) [FREE, OPEN, ASSIGNED]

-- stateToValue :: TaskState -> Value
-- stateToValue = ValueInt . fromEnum

-- readState :: Value -> TaskState
-- readState (ValueInt i) | i `elem` map fromEnum (enumFrom FREE) = toEnum i
-- readState _ = error "invalid task state"

listTasks :: Struct -- ^ opts
          -> Struct -- ^ qopts
          -> IO [Struct]
listTasks = koji "listTasks"
