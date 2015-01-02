--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Common helpers for git vogue plugins
module Common
(
    hsFiles,
    getPluginCommand,
    pCommand,
    PluginCommand(..),
) where

import           Control.Applicative
import           Data.List
import           Options.Applicative

-- | Filter the incoming file list by .hs files.
hsFiles :: IO [FilePath]
hsFiles = filter (isSuffixOf ".hs") . lines <$> getContents

-- | Arguments to the plugin
data PluginCommand
    -- | Check the project for problems.
    = CmdCheck
    -- | Fix problems in the project.
    | CmdFix
    -- | Report details.
    | CmdName

-- | Parser for plugin arguments
pluginCommandParser :: Parser PluginCommand
pluginCommandParser = subparser
    (  pCommand "name" CmdName "Get name of plugin"
    <> pCommand "check" CmdCheck "Check for problems"
    <> pCommand "fix" CmdFix "Try to fix problems"
    )

-- | Sub-command helper
pCommand :: String -> a -> String -> Mod CommandFields a
pCommand name ctor desc = command name (info (pure ctor) (progDesc desc))

-- | Get the plugin command requested given a header and a description
getPluginCommand :: String -> String -> IO PluginCommand
getPluginCommand hdr desc = execParser parser
  where
    parser = info (helper <*> pluginCommandParser)
        ( fullDesc
        <> progDesc desc
        <> header hdr)

