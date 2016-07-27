-- | Provide a VCS implementation for directories not under version control
module Git.Vogue.VCS.Null
( nullVCS,
) where

import           Control.Monad          (filterM)
import           Control.Monad.IO.Class
import           Git.Vogue.Types
import           System.Directory
import           System.FilePath

nullVCS :: MonadIO m => VCS m
nullVCS = VCS
  { getFiles    = nullGetFiles
  , installHook = nullInstallHook
  , removeHook  = nullRemoveHook
  , checkHook   = nullCheckHook
  , getTopLevel = nullGetTopLevel
  }

nullGetFiles :: MonadIO m => SearchMode -> m [FilePath]
nullGetFiles mode = case mode of
  FindSpecific fs -> existantFiles fs
  _               -> liftIO getAllFiles
  where
    existantFiles = filterM (liftIO . doesFileExist)

nullInstallHook :: MonadIO m => m ()
nullInstallHook =
  liftIO $ putStrLn "Warning, hooks can't be installed with the null VCS backend"

nullRemoveHook :: MonadIO m => m ()
nullRemoveHook =
  liftIO $ putStrLn "Warning, hooks can't be removed with the null VCS backend"

nullCheckHook :: Applicative m => m Bool
nullCheckHook = pure False

nullGetTopLevel :: MonadIO m => m FilePath
nullGetTopLevel = liftIO getCurrentDirectory

getAllFiles :: IO [FilePath]
getAllFiles = getCurrentDirectory >>= go
  where
    go dir = do
      (dirs, files) <- getFilesAndDirectories dir
      (files ++) . concat <$> mapM go dirs

getFilesAndDirectories :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndDirectories dir = do
  c <- fmap (dir </>) . filter (`notElem` ["..", "."]) <$> getDirectoryContents dir
  (,) <$> filterM doesDirectoryExist c <*> filterM doesFileExist c

