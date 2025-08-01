module State
  ( -- * global state
    State (..),
    emptyState,
    readState,
    modifyState,
    modifyState_,

    -- * vde state
    VdeState (..),
    getVdeCtlDir,

    -- * vm state
    VmState (..),
    readVmState,
    writeVmState,
    removeVm,
    getVmFilePath,
    listRunningVms,

    -- * IPs
    getNextIp,
  )
where

import Context
import Data.Aeson
import Data.ByteString.Lazy qualified
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Data.Text.IO qualified as T
import Net.IPv4 (IPv4)
import Net.IPv4 qualified as IPv4
import Options (VmName (..))
import StdLib
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
  )
import System.FileLock
import Utils (filterMapM)

-- global state

data State = State
  { vde :: Maybe VdeState,
    vms :: Map VmName VmState,
    nextIp :: IPv4
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

emptyState :: State
emptyState =
  State
    { vde = Nothing,
      vms = mempty,
      nextIp = fst ipRange
    }

-- every other state interaction is implemented in terms of this `modifyState` function
modifyState :: Context -> (State -> IO (State, a)) -> IO a
modifyState ctx action = do
  file <- getStateFile ctx
  withFileLock file Exclusive $ \_lock -> do
    contents <- T.readFile file
    let parsed =
          if contents == ""
            then emptyState
            else either error id (eitherDecode' (cs contents) :: Either String State)
    cleanedUp <- cleanUpVms ctx parsed
    (next, a) <- action cleanedUp
    if Map.null (next ^. #vms) && isNothing (next ^. #vde)
      then do
        removeFile file
      else do
        Data.ByteString.Lazy.writeFile file (encode (next :: State))
    pure a

modifyState_ :: Context -> (State -> IO State) -> IO ()
modifyState_ ctx action = modifyState ctx $ \state -> do
  new <- action state
  pure (new, ())

readState :: Context -> IO State
readState ctx = modifyState ctx $ \state -> do
  pure (state, state)

getStateFile :: Context -> IO FilePath
getStateFile ctx = do
  let dir = storageDir ctx
  createDirectoryIfMissing True dir
  pure $ dir </> "state.json"

-- * vde state

newtype VdeState = VdeState
  { pid :: Int64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

getVdeCtlDir :: Context -> IO FilePath
getVdeCtlDir ctx = do
  let ctlDir = storageDir ctx </> "vde1.ctl"
  createDirectoryIfMissing True ctlDir
  pure ctlDir

-- * vm state

data VmState = VmState
  { port :: Int,
    pid :: Int,
    ip :: IPv4
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

cleanUpVms :: Context -> State -> IO State
cleanUpVms ctx state = do
  running <- filterMapM isRunning (state ^. #vms)
  pure $ state & #vms .~ running
  where
    isRunning :: VmName -> VmState -> IO Bool
    isRunning vmName vmState = do
      isRunning <- doesDirectoryExist $ "/proc/" <> show (vmState ^. #pid :: Int)
      unless isRunning $ do
        T.putStrLn $ "WARN: cannot find process for vm: " <> vmNameToText vmName
        removeVmDir ctx vmName
      pure isRunning

readVmState :: Context -> VmName -> IO VmState
readVmState ctx vmName = do
  state <- readState ctx
  case Map.lookup vmName (state ^. #vms) of
    Nothing -> error "vm not found"
    Just vmState -> pure vmState

writeVmState :: Context -> VmName -> VmState -> IO ()
writeVmState ctx vmName vmState = do
  modifyState_ ctx $ \state -> do
    pure $ state & #vms %~ Map.insert vmName vmState

removeVm :: Context -> VmName -> IO ()
removeVm ctx vmName = do
  removeVmDir ctx vmName
  modifyState_ ctx $ \state -> do
    pure $ state & #vms %~ Map.delete vmName

listRunningVms :: Context -> IO [VmName]
listRunningVms ctx = do
  state <- readState ctx
  pure $ sort $ Map.keys $ state ^. #vms

removeVmDir :: Context -> VmName -> IO ()
removeVmDir ctx vmName = do
  removeDirectoryRecursive $ storageDir ctx </> "vms" </> cs (vmNameToText vmName)
  vmDirs <- listDirectory (storageDir ctx </> "vms")
  when (null vmDirs) $ do
    removeDirectoryRecursive (storageDir ctx </> "vms")

getVmFilePath :: Context -> VmName -> FilePath -> IO FilePath
getVmFilePath ctx vmName path = do
  let dir = storageDir ctx </> "vms" </> cs (vmNameToText vmName)
  createDirectoryIfMissing True dir
  pure $ dir </> path

-- * IPs

ipRange :: (IPv4, IPv4)
ipRange = (IPv4.fromOctets 10 0 0 2, IPv4.fromOctets 10 0 0 254)

getNextIp :: Context -> IO IPv4
getNextIp ctx = modifyState ctx $ \state -> do
  let findIp candidate
        | candidate > snd ipRange = findIp (fst ipRange)
        | candidate `elem` fmap (^. #ip) (state ^. #vms) = findIp (succ candidate)
        | otherwise = candidate
  let ip = findIp $ state ^. #nextIp
  pure (state & #nextIp .~ succ ip, ip)
