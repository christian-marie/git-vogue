--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Git.Vogue where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.List
import           Data.Monoid
import           Data.String.Utils
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Posix.Files
import           System.Process

import           Git.Vogue.Plugins
import           Git.Vogue.Types
import           Paths_git_vogue

-- | Options parsed from the command-line.
data VogueOptions = Options
    { optSearch  :: SearchMode
    , optCommand :: VogueCommand
    }
  deriving (Eq, Show)

-- | Commands, with parameters, to be executed.
data VogueCommand
    -- | Add git-vogue support to a git repository.
    = CmdInit { templatePath :: Maybe FilePath }
    -- | Verify that support is installed and plugins happen.
    | CmdVerify
    -- | List the plugins that git-vogue knows about.
    | CmdPlugins
    -- | Run check plugins on files in a git repository.
    | CmdRunCheck
    -- | Run fix plugins on files in a git repository.
    | CmdRunFix
  deriving (Eq, Show)

-- | Plugins that git-vogue knows about.
--   FIXME: this will become the fix/check modules

newtype Vogue m x = Vogue { vogue :: ReaderT [Plugin] m x }
  deriving ( Functor, Applicative, Monad
           , MonadTrans, MonadIO, MonadReader [Plugin] )

-- | Execute a Vogue program
runVogue
    :: [Plugin]
    -> Vogue m a
    -> m a
runVogue ps (Vogue act) = runReaderT act ps

-- | Execute a git-vogue command.
runCommand
    :: (MonadIO m, Functor m)
    => VogueCommand
    -> SearchMode
    -> Vogue m ()
runCommand CmdInit{..} _ = runWithRepoPath (gitAddHook templatePath)
runCommand CmdVerify   _ = runWithRepoPath (gitCheckHook runsVogue)
runCommand CmdPlugins  _ = listPlugins
runCommand CmdRunCheck search = runCheck search
runCommand CmdRunFix   search = runFix search

-- | Try to fix the broken things. We first do one pass to check what's broken,
-- then only run fix on those.
runFix :: (MonadIO m, Functor m) => SearchMode -> Vogue m ()
runFix sm = do
    -- See which plugins failed first
    rs <- ask >>= mapM (\x -> (x,) <$> executeCheck ioPluginExecutorImpl sm x)
    -- Now fix the failed ones only
    getWorst (executeFix ioPluginExecutorImpl sm) [ x | (x, Failure{}) <- rs ]
    >>= outputStatusAndExit

-- | Check for broken things.
runCheck :: MonadIO m => SearchMode -> Vogue m ()
runCheck sm =
    ask
    >>= getWorst (executeCheck ioPluginExecutorImpl sm)
    >>= outputStatusAndExit

-- | Find the git repository path and pass it to an action.
--
-- Throws an error if the PWD is not in a git repo.
runWithRepoPath
    :: MonadIO m
    => (FilePath -> m a)
    -> m a
runWithRepoPath action =
    -- Get the path to the git repo top-level directory.
    liftIO (readProcess "git" ["rev-parse", "--show-toplevel"] "")
    >>= action . strip

--- | Command string to insert into pre-commit hooks.
preCommitCommand :: String
preCommitCommand = "git-vogue check"

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
copyHookTemplateTo maybe_t hook = do
    template <- maybe (getDataFileName "templates/pre-commit") return maybe_t
    copyFile template hook
    perm <- getPermissions hook
    setPermissions hook $ perm { executable = True }

-- | Use a predicate to check a git commit hook.
gitCheckHook
    :: MonadIO m
    => (FilePath -> IO Bool)
    -> FilePath
    -> Vogue m ()
gitCheckHook p path = do
    let hook = path </> ".git" </> "hooks" </> "pre-commit"
    -- Check it exists (so openFile doesn't explode).
    exists <- liftIO . fileExist $ hook
    if exists
        then checkPredicate hook
        else failWith $ "Missing file " <> hook
    liftIO exitSuccess
  where
    checkPredicate hook = liftIO $ do
        pass <- p hook
        unless pass $ failWith "Invalid configuration."
    failWith msg = liftIO $ do
        hPutStrLn stderr msg
        exitFailure

-- | Check that a script seems to run git vogue.
runsVogue
    :: FilePath
    -> IO Bool
runsVogue path = do
    c <- readFile path
    return $ preCommitCommand `isInfixOf` c

-- | Print a list of all plugins.
listPlugins :: MonadIO m => Vogue m ()
listPlugins = do
  plugins <- ask
  liftIO .  putStrLn
         $  "git-vogue knows about the following plugins: "
         <> show plugins
