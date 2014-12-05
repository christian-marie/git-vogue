{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Git.Vogue where

import           Control.Monad.Base
import           Control.Monad.IO.Class      ()
import           Control.Monad.Trans.Control
import           Data.List
import           Data.Monoid
import           System.Directory
import           System.FilePath
import           System.Posix.Files
import           System.Process

import           Paths_git_vogue

-- | Command string to insert into pre-commit hooks.
preCommitCommand :: String
preCommitCommand = "git-vogue check"

-- | Commands, with parameters, to be executed.
data VogueCommand
    -- | Add git-vogue support to a git repository.
    = CmdInit
    -- | Verify that support is installed and plugins happen.
    | CmdVerify
    -- | Run check plugins on a git repository.
    | CmdRunCheck
    -- | Run fix plugins on a git repository.
    | CmdRunFix
  deriving (Eq, Show)

-- | Execute a git-vogue command.
runCommand
    :: VogueCommand
    -> IO ()
runCommand CmdInit = runWithRepoPath gitAddHook
runCommand CmdVerify = error "Not implemented: verify"
runCommand CmdRunCheck = error "Not implemented: check"
runCommand CmdRunFix = error "Not implemented: fix"

-- | Find the git repository path and pass it to an action.
--
-- Throws an error if the PWD is not in a git repo.
runWithRepoPath
    :: MonadBaseControl IO m
    => (FilePath -> m a)
    -> m a
runWithRepoPath action = do
    -- Get the path to the git repo top-level directory.
    git_repo <- liftBase $ readProcess "git" ["rev-parse", "--show-toplevel"] ""
    action $ trim git_repo

-- | Add the git pre-commit hook.
gitAddHook :: FilePath -> IO ()
gitAddHook path = do
    let hook = path </> ".git" </> "hooks" </> "pre-commit"
    putStrLn $ "Adding " <> preCommitCommand <> " to hooks in " <> path
    exists <- fileExist hook
    if exists
        then updateHook hook
        else createHook hook
  where
    updateHook hook = print ":-|"
    createHook hook = do
        template <- getDataFileName "templates/pre-commit"
        copyFile template hook
        perm <- getPermissions hook
        setPermissions hook $ perm { executable = True }

-- | Trim whitespace from a string.
trim :: String -> String
trim = (dropWhile ws) . (dropWhileEnd ws)
  where
    ws = (`elem` " \t\n")
