--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

module Git.Vogue.Plugins where

import           Git.Vogue.Types

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Monoid
import           Data.String
import           Data.String.Utils
import           Data.Text.Lazy         (Text)
import qualified Data.Text.Lazy         as T
import qualified Data.Text.Lazy.IO      as T
import           Formatting
import           Prelude                hiding (maximum)
import           System.Directory
import           System.Exit
import           System.Process

-- | Execute a plugin in IO
ioPluginExecutorImpl :: MonadIO m => PluginExecutorImpl m
ioPluginExecutorImpl =
    PluginExecutorImpl (f "fix") (f "check")
  where
    -- | Given the command sub-type, and the path to the plugin, execute it
    -- appropriately.
    --
    -- This involves the interface described in README under "Plugin design".
    f :: MonadIO m => String -> SearchMode -> Plugin -> m (Status a)
    f arg sm (Plugin path) = liftIO $ do
        name <- getName path
        fs <- unlines <$> (lines <$> paths sm >>= filterM doesFileExist)
        (status, out, err) <- readProcessWithExitCode path [arg] fs
        let glommed = fromString $ out <> err

        return $ case status of
            ExitSuccess   -> Success name glommed
            ExitFailure 1 -> Failure name glommed
            ExitFailure n -> Catastrophe n name glommed

    paths FindChanged = git ["diff", "--cached", "--name-only"]
    paths FindAll     = git ["ls-files"]

    git args = readProcess "git" args ""

    getName path = do
        (status, name, _) <- readProcessWithExitCode path ["name"] mempty
        return . PluginName . fromString . strip $ case status of
            ExitSuccess -> if null name then path else name
            ExitFailure _ -> path

colorize :: Status a -> Text
colorize (Success     (PluginName x) y) =
    format ("\x1b[32m" % text % " succeeded\x1b[0m with:\n" % text) x y
colorize (Failure     (PluginName x) y) =
    format ("\x1b[33m" % text % " failed\x1b[0m with:\n" % text) x y
colorize (Catastrophe n (PluginName x) y) =
    format ("\x1b[31m"
           % text
           % " exploded \x1b[0m with exit code "
           % int
           %":\n"
           % text) x n y

-- | Output the result of a Plugin and exit with an appropriate return code
outputStatusAndExit
    :: MonadIO m
    => Status a
    -> m ()
outputStatusAndExit status = liftIO $
    case status of
        Success _ output -> do
            T.putStrLn output
            exitSuccess
        Failure _ output -> do
            T.putStrLn output
            exitWith $ ExitFailure 1
        Catastrophe _ _ output -> do
            T.putStrLn output
            exitWith $ ExitFailure 2

-- | Run a bunch of plugin actions, mush the statuses together and stick them
-- all under the header of the worst.
getWorst
    :: Monad m
    => (Plugin -> m (Status a))
    -> [Plugin]
    -> m (Status a)
getWorst f ps = do
    rs <- mapM f ps
    return $ insertMax rs (T.unlines $ fmap colorize rs)

insertMax :: [Status a] -> Text -> Status a
insertMax [] _   = Success mempty "No plugins to run, vacuous success."
insertMax rs txt =
    case maximum rs of
        Success{} -> Success mempty txt
        Failure{} -> Failure mempty txt
        Catastrophe{} -> Catastrophe 0 mempty txt
