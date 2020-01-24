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
       , getBuild
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
---hub = "https://brewhub.engineering.redhat.com/brewhub"

koji :: Remote a => String -> a
koji c = remote hub c
--koji c = remoteWithHeaders hub c [("User-Agent", "koji/1")]

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

getPackageID :: String -> IO Int
getPackageID pkg =
  koji "getPackageID" pkg

getTagID :: String -> IO Int
getTagID tag =
  koji "getTagID" tag

type Struct = [(String,Value)]

listPackagesSimple :: String -> IO [Struct]
listPackagesSimple prefix = do
  koji "listPackagesSimple" prefix

listTags :: IO Value
listTags =
  koji "listTags"

listTagsBuild :: Int -> IO Value
listTagsBuild buildid =
  koji "listTags" buildid ()

listTagsPackage :: Int -> IO Value
listTagsPackage pkgid =
  koji "listTags" () pkgid

checkTagPackage :: Int -> Int -> IO Bool
checkTagPackage tagid pkgid =
  koji "checkTagPackage" tagid pkgid

getLatestBuild :: String -> String -> IO Value
getLatestBuild tag pkg =
  koji "getLatestBuilds" tag () pkg

-- data TaskState = FREE | OPEN | CLOSED | CANCELED | ASSIGNED | FAILED
--   deriving (Eq, Enum)

--data TaskInfo = TaskInfo String TaskState

getTaskInfo :: Int -> IO Struct
getTaskInfo taskid =
  koji "getTaskInfo" taskid
  -- res <- kojiCall "getTaskInfo" [show taskid]
  -- let state = res ^? key "state" % _Integer <&> (toEnum . fromInteger)
  --     arch = res ^? key "arch" % _String
  -- return $ TaskInfo arch state

getBuild :: Int -> IO Struct
getBuild buildid =
  koji "getBuild" buildid

data TaskState = FREE | OPEN | CLOSED | CANCELED | ASSIGNED | FAILED
  deriving (Eq, Enum)

openstates :: Value
openstates = ValueArray $ map (ValueInt . fromEnum) [FREE, OPEN, ASSIGNED]

stateToValue :: TaskState -> Value
stateToValue = ValueInt . fromEnum

readState :: Value -> TaskState
readState (ValueInt i) | i `elem` (map fromEnum (enumFrom FREE)) = toEnum i
readState _ = error "invalid task state"

listTasks :: Struct -> Struct -> IO [Struct]
listTasks opts qopts = koji "listTasks" opts qopts

