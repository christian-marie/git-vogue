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
    gitVCS,
    git
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List
import           Data.Monoid
import           Data.String.Utils
import           System.Directory
import           System.FilePath
import           System.Posix.Files
import           System.Process

import           Git.Vogue.Types
import           Paths_git_vogue

gitVCS :: (Functor m, MonadIO m) => VCS m
gitVCS = VCS
    { getFiles     = gitGetFiles
    , installHook  = gitAddHook
    , removeHook   = gitRemoveHook
    , checkHook    = gitCheckHook
    }

gitGetFiles
    :: MonadIO m
    => SearchMode
    -> m [FilePath]
gitGetFiles mode = liftIO . existantFiles $
    case mode of FindChanged -> git ["diff", "--cached", "--name-only"]
                 FindAll     -> git ["ls-files"]
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
    template <- getDataFileName "templates/pre-commit"
    hook <- gitHookFile
    copyFile template hook
    perm <- getPermissions hook
    setPermissions hook $ perm { executable = True }

-- | Remove the hook iff it is precicely the same as the template.
gitRemoveHook
    :: MonadIO m
    => m ()
gitRemoveHook = liftIO $ do
    template_contents <- getDataFileName "templates/pre-commit" >>= readFile
    hook <- gitHookFile
    hook_contents <- readFile hook
    if template_contents == hook_contents then
        removeFile hook
    else
        putStrLn $ "Your pre-commit hook appears to be modified. \n"
                <> "Please manually remove:" <> hook

-- | Use a predicate to check a git commit hook.
gitCheckHook
    :: MonadIO m
    => m Bool
gitCheckHook = liftIO $ do
    hook <- gitHookFile
    exists <- fileExist hook
    if exists
        then do
            c <- readFile hook
            unless (preCommitCommand `isInfixOf` c) .
                putStrLn $ "A pre-commit hook already exists at \n\t"
                    <> hook
                    <> "\nbut it does not contain the command\n\t"
                    <> preCommitCommand
                    <> "\nPlease edit the hook and add this command yourself!"
            return True
        else return False

-- | Where the pre-commit hook lives
gitHookFile :: IO FilePath
gitHookFile = do
    top_level <- strip <$> git ["rev-parse", "--show-toplevel"]
    return $ top_level </> ".git" </> "hooks" </> "pre-commit"
