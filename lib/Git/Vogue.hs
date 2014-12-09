{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

module Git.Vogue where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Posix.Files
import           System.Process

import           Git.Vogue.Modules
import           Git.Vogue.Types
import           Paths_git_vogue

-- | Command string to insert into pre-commit hooks.
preCommitCommand :: String
preCommitCommand = "git-vogue check"

-- | Commands, with parameters, to be executed.
data VogueCommand
    -- | Add git-vogue support to a git repository.
    = CmdInit { templatePath :: Maybe FilePath }
    -- | Verify that support is installed and plugins happen.
    | CmdVerify
    -- | List the plugins that git-vogue knows about.
    | CmdList
    -- | Run check plugins on a git repository.
    | CmdRunCheck
    -- | Run fix plugins on a git repository.
    | CmdRunFix
  deriving (Eq, Show)

-- | Plugins that git-vogue knows about.
--   FIXME: this will become the fix/check modules

newtype Vogue m x = Vogue { vogue :: ReaderT [Plugin] m x }
  deriving ( Functor, Applicative, Monad
           , MonadTrans, MonadIO )

-- | Execute a Vogue program
runVogue
    :: [Plugin]
    -> Vogue m a
    -> m a
runVogue ps (Vogue act) = runReaderT act ps

-- | Execute a git-vogue command.
runCommand
    :: MonadIO m
    => VogueCommand
    -> Vogue m ()
runCommand CmdInit{..} = runWithRepoPath (gitAddHook templatePath)
runCommand CmdVerify   = error "Not implemented: verify"
runCommand CmdList     = gitListHook
runCommand CmdRunCheck = do
    plugins <- Vogue ask
    liftIO $ checkModules' plugins
runCommand CmdRunFix   = do
    plugins <- Vogue ask
    liftIO $ fixModules' plugins

-- | Find the git repository path and pass it to an action.
--
-- Throws an error if the PWD is not in a git repo.
runWithRepoPath
    :: MonadIO m
    => (FilePath -> m a)
    -> m a
runWithRepoPath action = do
    -- Get the path to the git repo top-level directory.
    git_repo <- liftIO $ readProcess "git" ["rev-parse", "--show-toplevel"] ""
    action $ trim git_repo

-- | Add the git pre-commit hook.
gitAddHook
    :: MonadIO m
    => Maybe FilePath -- ^ Template path
    -> FilePath       -- ^ Hook path
    -> Vogue m ()
gitAddHook template path = liftIO $ do
    let hook = path </> ".git" </> "hooks" </> "pre-commit"
    exists <- fileExist hook
    if exists
        then updateHook hook
        else createHook hook
  where
    createHook = copyHookTemplateTo template
    updateHook hook = do
        content <- readFile hook
        unless (preCommitCommand `isInfixOf` content) $ do
            putStrLn $ "A pre-commit hook already exists at \n\t"
                <> hook
                <> "\nbut it does not contain the command\n\t"
                <> preCommitCommand
                <> "\nPlease edit the hook and add this command yourself!"
            exitFailure
        putStrLn "Your commit hook is already in place."

-- | Copy the template pre-commit hook to a git repo.
copyHookTemplateTo
    :: Maybe FilePath
    -> FilePath
    -> IO ()
copyHookTemplateTo use_template hook = do
    default_template <- getDataFileName "templates/pre-commit"
    let template = fromMaybe default_template use_template
    copyFile template hook
    perm <- getPermissions hook
    setPermissions hook $ perm { executable = True }

gitListHook :: MonadIO m => Vogue m ()
gitListHook = do
  plugins <- Vogue ask
  liftIO $  putStrLn
         $  "git-vogue knows about the following plugins: "
         <> show plugins

-- | Trim whitespace from a string.
trim :: String -> String
trim = dropWhile ws . dropWhileEnd ws
  where
    ws = (`elem` " \t\n")
