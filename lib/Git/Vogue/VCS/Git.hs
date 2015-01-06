--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

 -- | Provide a VCS implementation for git repositories
module Git.Vogue.VCS.Git
(
    gitVCS
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List
import           Data.Monoid
import           Data.String
import           Data.String.Utils
import           Paths_git_vogue
import           Prelude                hiding (maximum)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Posix.Files
import           System.Process

import           Git.Vogue.Types

gitVCS :: (Functor m, MonadIO m) => VCS m
gitVCS = VCS
    { getFiles     = gitGetFiles
    , installHook  = gitAddHook
    , removeHook   = error "remove hook unimplemented"
    , checkHook    = gitCheckHook
    , findTopLevel = strip <$> git ["rev-parse", "--show-toplevel"]
    }

gitGetFiles
    :: MonadIO m
    => SearchMode
    -> m [FilePath]
gitGetFiles mode = liftIO . existantFiles $
    case mode of FindAll     -> git ["diff", "--cached", "--name-only"]
                 FindChanged -> git ["ls-files"]
  where
    existantFiles f = lines <$> f >>= filterM doesFileExist

git :: MonadIO m => [String] -> m String
git args = liftIO $ readProcess "git" args ""

--- | Command string to insert into pre-commit hooks.
preCommitCommand :: String
preCommitCommand = "git-vogue check"

-- | Add the git pre-commit hook.
gitAddHook
    :: MonadIO m
    => m ()
gitAddHook = liftIO $ do
    let hook = ".git" </> "hooks" </> "pre-commit"
    exists <- fileExist hook
    if exists
        then updateHook hook
        else createHook hook
  where
    createHook = copyHookTemplateTo
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
    :: FilePath
    -> IO ()
copyHookTemplateTo hook = do
    template <- getDataFileName "templates/pre-commit"
    copyFile template hook
    perm <- getPermissions hook
    setPermissions hook $ perm { executable = True }

-- | Use a predicate to check a git commit hook.
gitCheckHook
    :: MonadIO m
    => m Bool
gitCheckHook = liftIO $ do
    exists <- fileExist gitHookFile
    if exists
        then do
            c <- readFile gitHookFile
            return $ preCommitCommand `isInfixOf` c
        else return False

-- | Where the pre-commit hook lives
gitHookFile :: FilePath
gitHookFile = ".git" </> "hooks" </> "pre-commit"
