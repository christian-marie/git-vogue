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

-- | Phantom type for Statuses related to checking
data Check
-- | Phantom type for Statuses related to fixing
data Fix

-- | Result of running a Plugin
data Status a
    = Success PluginName Text
    | Failure PluginName Text
    | Catastrophe Int PluginName Text
  deriving (Show, Ord, Eq)

-- | Absolute path to an executable
newtype Plugin = Plugin {
    unPlugin :: FilePath
} deriving (Show, Ord, Eq, IsString)

-- | Nice, human readable name of a plugin
newtype PluginName = PluginName {
    unPluginName :: Text
} deriving (Show, Ord, Eq, IsString, Monoid)

-- | We want the flexibility of just checking changed files, or maybe checking
-- all of them.
data SearchMode = FindAll | FindChanged

-- | An implementation of a "runner" of plugins. Mostly for easy testing.
data PluginExecutorImpl m = PluginExecutorImpl{
    executeFix   :: SearchMode -> Plugin -> m (Status Fix),
    executeCheck :: SearchMode -> Plugin -> m (Status Check)
}
