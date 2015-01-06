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

module Git.Vogue.Types where

import           Data.Monoid
import           Data.String
import           Data.Text.Lazy (Text)

-- | Options parsed from the command-line.
data VogueOptions = Options
    { optSearch  :: SearchMode
    , optCommand :: VogueCommand
    }
  deriving (Eq, Show)

-- | Commands, with parameters, to be executed.
data VogueCommand
    -- | Add git-vogue support to a git repository.
    = CmdInit 
    -- | Verify that support is installed and plugins happen.
    | CmdVerify
    -- | List the plugins that git-vogue knows about.
    | CmdPlugins
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
    { pluginName :: Text
    , enabled    :: Bool
    , runCheck   :: [FilePath] -> m Result
    , runFix     :: [FilePath] -> m Result
    }

newtype PluginName = PluginName {
    unPluginName :: Text
} deriving (Show, Ord, Eq, IsString, Monoid)

-- | We want the flexibility of just checking changed files, or maybe checking
-- all of them.
data SearchMode = FindAll | FindChanged
  deriving (Eq, Show)

-- | A thing that can find plugins, for example we might search through the
-- libexec directory for executables.
data PluginDiscoverer m = PluginDiscoverer
    { discoverPlugins :: m [Plugin m]
    , disablePlugin   :: PluginName -> m [Plugin m]
    , enablePlugin    :: PluginName -> m ()
    }

-- | A VCS backend, such as git.
data VCS m = VCS
    { getFiles     :: SearchMode -> m [FilePath] -- ^ Find all staged files
    , installHook  :: m ()                       -- ^ Install pre-commit hook
    , removeHook   :: m ()                       -- ^ Remove pre-commit hook
    , checkHook    :: m Bool                     -- ^ Check pre-commit hook
    , findTopLevel :: m FilePath                 -- ^ Find the / of the repo
    }
