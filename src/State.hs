module State
  ( VmState (..),
    writeState,
    readState,
    getStateDir,
    getStateFile,
    listRunningVms,
    removeState,

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
import Data.Int (Int64)
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
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

listRunningVms :: Context -> IO [VmName]
listRunningVms ctx = do
  createDirectoryIfMissing True (storageDir ctx)
  vms <-
    listDirectory (storageDir ctx)
      <&> fmap (VmName . cs)
      -- todo: put vm dirs in subdirectory
      -- todo: put vde paths in subdirectory
      . filter (\dir -> dir `notElem` ["vde.json", "vde1.ctl"])
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
