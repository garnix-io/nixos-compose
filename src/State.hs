module State
  ( VmState (..),
    writeState,
    readState,
    getStateDir,
    getStateFile,
    listRunningVms,
    removeStateDir,

    -- * vde state
    VdeState (..),
    readVdeState,
    modifyVdeState,
    getVdeCtlDir,
  )
where

import Context
import Control.Monad (filterM)
import Data.Aeson
import Data.ByteString.Lazy qualified
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

-- * vm state

data VmState = VmState
  { port :: Int,
    pid :: Maybe Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

listRunningVms :: Context -> IO [VmName]
listRunningVms ctx = do
  exists <- doesDirectoryExist (storageDir ctx </> "vms")
  if not exists
    then pure []
    else do
      vms <- fmap (VmName . cs) <$> listDirectory (storageDir ctx </> "vms")
      filterM (isRunning ctx) vms

isRunning :: Context -> VmName -> IO Bool
isRunning ctx vmName = do
  state <- readState ctx vmName
  isRunning <- case state ^. #pid of
    Nothing -> pure False
    Just pid -> doesDirectoryExist $ "/proc/" <> show pid
  unless isRunning $ do
    T.putStrLn $ "WARN: cannot find process for vm: " <> vmNameToText vmName
    removeStateDir ctx vmName
  pure isRunning

writeState :: Context -> VmName -> VmState -> IO ()
writeState ctx vmName state = do
  path <- getStateFile ctx vmName "state.json"
  encodeFile path state

readState :: Context -> VmName -> IO VmState
readState ctx vmName = do
  path <- getStateFile ctx vmName "state.json"
  either error id <$> eitherDecodeFileStrict' path

removeStateDir :: Context -> VmName -> IO ()
removeStateDir ctx vmName = do
  removeDirectoryRecursive =<< getStateDir ctx vmName
  vmDirs <- listDirectory (storageDir ctx </> "vms")
  when (null vmDirs) $ do
    removeDirectoryRecursive (storageDir ctx </> "vms")

getStateFile :: Context -> VmName -> FilePath -> IO FilePath
getStateFile ctx vmName path = getStateDir ctx vmName <&> (</> path)

getStateDir :: Context -> VmName -> IO FilePath
getStateDir ctx (VmName vmName) = do
  let dir = storageDir ctx </> "vms" </> cs vmName
  createDirectoryIfMissing True dir
  pure dir

-- global vde switch state

newtype VdeState = VdeState
  { pid :: Int64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

readVdeState :: Context -> IO (Maybe VdeState)
readVdeState ctx = do
  file <- getVdeFile ctx
  withFileLock file Shared $ \_lock -> do
    contents <- T.readFile file
    pure $
      if contents == ""
        then Nothing
        else Just (either error id (eitherDecode' (cs contents) :: Either String VdeState))

modifyVdeState :: Context -> (Maybe VdeState -> IO (Maybe VdeState)) -> IO ()
modifyVdeState ctx action = do
  file <- getVdeFile ctx
  withFileLock file Exclusive $ \_lock -> do
    contents <- T.readFile file
    let previous =
          if contents == ""
            then Nothing
            else Just (either error id (eitherDecode' (cs contents) :: Either String VdeState))
    next <- action previous
    case next of
      Just next -> Data.ByteString.Lazy.writeFile file (encode (next :: VdeState))
      Nothing -> removeFile file

getVdeFile :: Context -> IO FilePath
getVdeFile ctx = do
  let dir = storageDir ctx
  createDirectoryIfMissing True dir
  pure $ dir </> "vde.json"

getVdeCtlDir :: Context -> IO FilePath
getVdeCtlDir ctx = do
  let ctlDir = storageDir ctx </> "vde1.ctl"
  createDirectoryIfMissing True ctlDir
  pure ctlDir
