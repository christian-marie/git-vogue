--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module Git.Vogue.Types where

import           Data.Function
import           Data.Monoid
import           Data.Ord
import           Data.String
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

-- | Options parsed from the command-line.
data VogueOptions = Options
    { optSearch  :: SearchMode
    , optCommand :: VogueCommand
    , optDisable :: [PluginName]
    , optVCS     :: VCSType
    }
  deriving (Eq, Show)

data VCSType = Git | Null
  deriving (Eq, Show)

-- | Commands, with parameters, to be executed.
data VogueCommand
    -- | Add git-vogue support to a git repository.
    = CmdInit
    -- | Verify that support is installed and plugins happen.
    | CmdVerify
    -- | List the plugins that git-vogue knows about.
    | CmdPlugins
    -- | Disable a plugin
    | CmdDisable PluginName
    -- | Enable a plugin
    | CmdEnable PluginName
    -- | Run check plugins on files in a git repository.
    | CmdRunCheck
    -- | Run fix plugins on files in a git repository.
    | CmdRunFix
  deriving (Eq, Show)


-- | Phantom type for Statuses related to checking
data Check
-- | Phantom type for Statuses related to fixing
data Fix

-- | Result of running a Plugin
data Result
    = Success Text
    | Failure Text
    | Catastrophe Int Text
  deriving (Show, Ord, Eq)

-- | A plugin that can be told to check or fix a list of files
data Plugin m = Plugin
    { pluginName :: PluginName
    , enabled    :: Bool
    , runCheck   :: [FilePath] -> [FilePath] -> m Result
    , runFix     :: [FilePath] -> [FilePath] -> m Result
    }

instance Show (Plugin m) where
    show Plugin{..} =
        T.unpack (unPluginName pluginName)
            <> if enabled then mempty else " (disabled)"

instance Eq (Plugin m) where
    (==) = (==) `on` pluginName

instance Ord (Plugin m) where
    compare = comparing pluginName

newtype PluginName = PluginName {
    unPluginName :: Text
} deriving (Show, Ord, Eq, IsString, Monoid)

-- | We want the flexibility of just checking changed files, or maybe checking
-- all of them.
data SearchMode
    = FindAll
    | FindChanged
    | FindSpecific [FilePath]
  deriving (Eq, Show)

-- | A thing that can find plugins, for example we might search through the
-- libexec directory for executables.
data PluginDiscoverer m = PluginDiscoverer
    { discoverPlugins :: m [Plugin m]
    , disablePlugin   :: PluginName -> m ()
    , enablePlugin    :: PluginName -> m ()
    }

-- | A VCS backend, such as git.
data VCS m = VCS
    { getFiles    :: SearchMode -> m [FilePath] -- ^ Find all staged files
    , installHook :: m ()                       -- ^ Install pre-commit hook,
                                                --   will only be called if
                                                --   checkHook returns False
    , removeHook  :: m ()                       -- ^ Remove pre-commit hook
    , checkHook   :: m Bool                     -- ^ Check pre-commit hook
    , getTopLevel :: m FilePath                 -- ^ Find the / of the repo
    }
