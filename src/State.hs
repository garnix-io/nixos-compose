module State where

import Context
import Control.Monad (filterM)
import Data.Aeson
import Data.String.Conversions (cs)
import Data.Text.IO qualified as T
import Options (VmName (..))
import StdLib
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
    removeDirectoryRecursive,
  )
import System.FilePath ((</>))

data VmState = VmState
  { port :: Int,
    pid :: Maybe Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

listRunningVms :: Context -> IO [VmName]
listRunningVms ctx = do
  createDirectoryIfMissing True (storageDir ctx)
  vms <- fmap (VmName . cs) <$> listDirectory (storageDir ctx)
  filterM (isRunning ctx) vms

isRunning :: Context -> VmName -> IO Bool
isRunning ctx vmName = do
  state <- readState ctx vmName
  isRunning <- case state ^. #pid of
    Nothing -> pure False
    Just pid -> doesDirectoryExist $ "/proc/" <> show pid
  unless isRunning $ do
    T.putStrLn $ "WARN: cannot find process for vm: " <> vmNameToText vmName
    getStateDir ctx vmName >>= removeDirectoryRecursive
  pure isRunning

writeState :: Context -> VmName -> VmState -> IO ()
writeState ctx vmName state = do
  path <- getStateFile ctx vmName "state.json"
  encodeFile path state

readState :: Context -> VmName -> IO VmState
readState ctx vmName = do
  path <- getStateFile ctx vmName "state.json"
  either error id <$> eitherDecodeFileStrict' path

removeState :: Context -> VmName -> IO ()
removeState ctx vmName = do
  removeDirectoryRecursive =<< getStateDir ctx vmName

getStateFile :: Context -> VmName -> FilePath -> IO FilePath
getStateFile ctx vmName path = getStateDir ctx vmName <&> (</> path)

getStateDir :: Context -> VmName -> IO FilePath
getStateDir ctx (VmName vmName) = do
  let dir = storageDir ctx </> cs vmName
  createDirectoryIfMissing True dir
  pure dir
