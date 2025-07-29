module State
  ( -- * global state
    State (..),
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
  )
where

import Context
import Data.Aeson
import Data.ByteString.Lazy qualified
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.Conversions (cs)
import Data.Text.IO qualified as T
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
import System.FilePath ((</>))
import Utils (filterMapM)

-- global state

data State = State
  { vde :: VdeState,
    vms :: Map VmName VmState
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

readState :: Context -> IO (Maybe State)
readState ctx = do
  file <- getStateFile ctx
  withFileLock file Shared $ \_lock -> do
    contents <- T.readFile file
    pure $
      if contents == ""
        then Nothing
        else Just (either error id (eitherDecode' (cs contents) :: Either String State))

modifyState :: Context -> (Maybe State -> IO (Maybe State, a)) -> IO a
modifyState ctx action = do
  file <- getStateFile ctx
  withFileLock file Exclusive $ \_lock -> do
    contents <- T.readFile file
    let previous =
          if contents == ""
            then Nothing
            else Just (either error id (eitherDecode' (cs contents) :: Either String State))
    (next, a) <- action previous
    case next of
      Just next -> Data.ByteString.Lazy.writeFile file (encode (next :: State))
      Nothing -> removeFile file
    pure a

modifyState_ :: Context -> (Maybe State -> IO (Maybe State)) -> IO ()
modifyState_ ctx action = modifyState ctx $ \state -> do
  new <- action state
  pure (new, ())

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
    pid :: Maybe Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

readVmState :: Context -> VmName -> IO VmState
readVmState ctx vmName = do
  state <- readState ctx
  case state of
    Nothing -> error "no state file found"
    Just state -> do
      case Map.lookup vmName (state ^. #vms) of
        Nothing -> error "vm not found"
        Just vmState -> pure vmState

writeVmState :: Context -> VmName -> VmState -> IO ()
writeVmState ctx vmName vmState = do
  modifyState_ ctx $ \case
    Nothing -> error "no state file found"
    Just state -> do
      pure $ Just $ state & #vms %~ Map.insert vmName vmState

removeVm :: Context -> VmName -> IO ()
removeVm ctx vmName = do
  removeVmDir ctx vmName
  modifyState_ ctx $ \case
    Nothing -> pure Nothing
    Just state -> pure $ Just $ state & #vms %~ Map.delete vmName

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

listRunningVms :: Context -> IO [VmName]
listRunningVms ctx = modifyState ctx $ \state -> do
  case state of
    Nothing -> pure (Nothing, [])
    Just state -> do
      running <- filterMapM isRunning (state ^. #vms)
      pure (Just $ state & #vms .~ running, Map.keys running)
  where
    isRunning :: VmName -> VmState -> IO Bool
    isRunning vmName vmState = do
      isRunning <- case vmState ^. #pid of
        Nothing -> pure False
        Just pid -> doesDirectoryExist $ "/proc/" <> show pid
      unless isRunning $ do
        T.putStrLn $ "WARN: cannot find process for vm: " <> vmNameToText vmName
        removeVmDir ctx vmName
      pure isRunning
