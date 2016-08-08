--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

module Git.Vogue where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Maybe
import           Data.Text.Lazy         (Text)
import qualified Data.Text.Lazy.IO      as T
import           Data.Traversable       hiding (sequence)
import           Formatting
import           Prelude                hiding (elem, maximum)
import           System.Directory
import           System.Exit

import           Git.Vogue.Types

-- | Execute a git-vogue command.
runCommand
    :: forall m. (Applicative m, MonadIO m, Functor m)
    => VogueCommand
    -> SearchMode
    -> [PluginName]
    -- ^ Disabled plugins
    -> VCS m
    -> PluginDiscoverer m
    -> m ()
runCommand cmd search_mode disabled_plugins VCS{..} PluginDiscoverer{..} =
    go cmd
  where
    cd = getTopLevel >>= liftIO . setCurrentDirectory
    go CmdInit = do
        cd
        already_there <- checkHook
        if already_there
            then success "Pre-commit hook is already installed"
            else do
                installHook
                installed <- checkHook
                if installed
                    then success "Successfully installed hook"
                    else failure "Hook failed to install"

    go CmdVerify = do
        cd
        installed <- checkHook
        if installed
            then success "Pre-commit hook currently installed"
            else failure "Pre-commit hook not installed"

    go CmdPlugins = do
        liftIO $  T.putStrLn "git-vogue knows about the following plugins:\n"
        discoverPlugins >>= liftIO . traverse_ print

    go (CmdDisable plugin) = do
        plugins <- discoverPlugins
        if plugin `elem` fmap pluginName (filter (not . enabled) plugins)
            then success "Plugin already disabled"
            else
                if plugin `elem` fmap pluginName plugins
                    then do
                        disablePlugin plugin
                        success "Disabled plugin"
                    else
                        failure "Unknown plugin"


    go (CmdEnable plugin) = do
        ps <- discoverPlugins
        if plugin `elem` fmap pluginName ps
            then
                if plugin `elem` (pluginName <$> filter (not . enabled) ps)
                    then do
                        enablePlugin plugin
                        success "Enabled plugin"
                    else
                        success "Plugin already enabled"
            else
                failure "Unknown plugin"

    go CmdRunCheck = do
        (check_fs, all_fs, plugins) <- things
        rs <- for plugins $ \p -> do
            r <- runCheck p check_fs all_fs
            liftIO . T.putStrLn $ colorize p r
            return r
        exitWithWorst rs


    go CmdRunFix = do
        (check_fs, all_fs, plugins) <- things
        rs <- for plugins $ \p -> do
            r <- runCheck p check_fs all_fs
            case r of
                Failure{} -> do
                    r' <- runFix p check_fs all_fs
                    liftIO . T.putStrLn $ colorize p r'
                    return $ Just r'
                _  -> return Nothing

        exitWithWorst (catMaybes rs)

    things = do
        cd
        check_fs <- getFiles search_mode
        when (null check_fs) (success "Vacuous success - Nothing to check")

        plugins <- filter ((`notElem` disabled_plugins) . pluginName)
                 . filter enabled
               <$> discoverPlugins
        when (null check_fs) (success "Vacuous success - No plugins enabled")

        all_fs <- getFiles FindAll
        return (check_fs, all_fs, plugins)

success, failure :: MonadIO m => Text -> m a
success msg = liftIO (T.putStrLn msg >> exitSuccess)
failure msg = liftIO (T.putStrLn msg >> exitFailure)

-- | Output the results of a run and exit with an appropriate return code
exitWithWorst
    :: MonadIO m
    => [Result]
    -> m ()
exitWithWorst [] = liftIO exitSuccess
exitWithWorst rs = liftIO $
    case maximum rs of
        Success{}     -> exitSuccess
        Failure{}     -> exitWith $ ExitFailure 1
        Catastrophe{} -> exitWith $ ExitFailure 2

colorize
    :: Plugin a
    -> Result
    -> Text
colorize Plugin{..} (Success txt) =
    format ("\x1b[32m"
            % text
            % " succeeded\x1b[0m with:\n"
            % text) (unPluginName pluginName) txt
colorize Plugin{..} (Failure txt) =
    format ("\x1b[31m"
            % text
            % " failed\x1b[0m with:\n"
            % text) (unPluginName pluginName) txt
colorize Plugin{..} (Catastrophe txt ret) =
    format ("\x1b[31m"
        % text
        % " exploded \x1b[0m with exit code "
        % int
        %":\n"
        % text) (unPluginName pluginName) txt ret
