--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

module Git.Vogue.PluginDiscoverer.Libexec
(
    libExecDiscoverer
) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text.Lazy         (Text)
import qualified Data.Text.Lazy         as T
import           Data.Traversable
import           Extra                  (withTempFile)
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.Process

import           Git.Vogue.Types
import           Git.Vogue.VCS.Git      (git)

libExecDiscoverer :: (Functor m, Applicative m, MonadIO m)
                  => FilePath
                  -> PluginDiscoverer m
libExecDiscoverer libexec_dir =
    PluginDiscoverer (discover libexec_dir) disable enable

-- | Find all plugins within the libexec dir.
--
-- This function inspects the $PREFIX/libexec/git-vogue directory and the
-- directories listed in the $GIT_VOGUE_PATH environmental variable (if
-- defined) and builds a 'Plugin' for the executables found.
--
-- Files that are set non executable are a corner case, this is not the
-- recommended way of disabling things.
--
-- Files that are in the git config's vogue.disable list are set disabled.
discover
    :: (Functor m, Applicative m, MonadIO m)
    => FilePath
    -> m [Plugin m]
discover libexec_dir = do
    let libexec_plugins = libexec_dir </> "git-vogue"

    -- Check if libexec dir does not exist, and notify user that this may be
    -- caused by cabal being dumb.
    is_dir <- liftIO $ doesDirectoryExist libexec_plugins
    unless is_dir . liftIO .
        putStrLn $ "Could not find libexec plugins: " <> libexec_plugins
                 <> "\nThis could be caused by installing with a buggy cabal, \
                    \see:\n\thttps://github.com/anchor/git-vogue/issues/80"

    -- Use the environmental variable and $libexec/git-vogue/ directories as
    -- the search path.
    path <- fromMaybe "" <$> liftIO (lookupEnv "GIT_VOGUE_PATH")

    let directories = splitOn ":" path <> [libexec_plugins]

    -- Disable plugins by the name that they present, so that the user does not
    -- need to know how the backend works.
    disabled <- gitDisabled
    ps <- (concat <$> traverse ls directories) >>= traverse (load disabled)
    return $ sort ps
  where
    load :: (Functor m, MonadIO m) => [Text] -> FilePath -> m (Plugin m)
    load disabled fp = do
        is_x <- executable <$> liftIO (getPermissions fp)
        if is_x
            then do
                -- Extract the plugin name
                name <- T.strip . T.pack <$> run fp "name"
                if name `elem` disabled
                    then return $ disabledPlugin name
                    else return $ enabledPlugin fp name
            else
                -- Corner case, if it's not executable we should just give it
                -- the name of the path and show it as a disabled plugin.
                return . disabledPlugin $ "(non-executable) " <> T.pack fp

    run :: MonadIO m => FilePath -> String -> m String
    run fp cmd = liftIO $ readProcess fp [cmd] ""

    -- | Build a Plugin that is ready to be executed.
    enabledPlugin :: MonadIO m => FilePath -> Text -> Plugin m
    enabledPlugin fp name =
        Plugin { pluginName = PluginName name
               , enabled    = True
               , runCheck   = runPlugin fp "check"
               , runFix     = runPlugin fp "fix"
               }

    disabledPlugin :: Text -> Plugin m
    disabledPlugin txt =
        Plugin { pluginName = PluginName txt
               , enabled    = False
               , runCheck   = error "disabled plugin ran check"
               , runFix     = error "disabled plugin ran fix"
               }

    ls :: (Functor m, MonadIO m) => FilePath -> m [FilePath]
    ls p = do
        exists <- liftIO $ doesDirectoryExist p
        if exists
            then (fmap . fmap) (p </>) (liftIO $ getDirectoryContents p)
                  >>= liftIO . filterM doesFileExist
            else return []

-- Given a path to the plugin, the appropriate sub-command (check or fix),
-- provide a function from list of files to status.
--
-- This involves the interface described in README under "Plugin design".
runPlugin
    :: MonadIO m
    => FilePath
    -> String
    -> [FilePath]
    -> [FilePath]
    -> m Result
runPlugin plugin cmd check_fs all_fs = liftIO $ do
    (status, out, err) <- withTempFile (\checkFileListPath ->
        withTempFile (\allFileListPath -> do
          writeFile checkFileListPath (show check_fs)
          writeFile allFileListPath (show all_fs)
          readProcessWithExitCode plugin [cmd, checkFileListPath, allFileListPath] ""
        )
      )
    let glommed = fromString $ out <> err
    return $ case status of
        ExitSuccess   -> Success glommed
        ExitFailure 1 -> Failure glommed
        ExitFailure n -> Catastrophe n glommed

-- | Get list of disabled plugins from git configuration.
gitDisabled
    :: (Monad m, Functor m, MonadIO m)
    => m [Text]
gitDisabled = T.lines . T.pack <$> liftIO (readConfig `catch` none)
  where
    readConfig = git ["config", "--get-all", "vogue.disable"]
    none (SomeException _) = return []

-- | Disable a given plugin within the libexec dir.
disable
    :: (Functor m, MonadIO m)
    => PluginName
    -> m ()
disable (PluginName name) =
    void $ git ["config", "--add", "vogue.disable", T.unpack name]

-- | Enable a given plugin within the libexec dir.
enable
    :: (Functor m, MonadIO m)
    => PluginName
    -> m ()
enable (PluginName name) =
    void $ git ["config", "--unset", "vogue.disable", T.unpack name]
