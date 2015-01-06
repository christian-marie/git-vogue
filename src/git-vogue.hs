--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.List.Split
import           Data.Maybe
import           Data.String
import           Data.Traversable
import           Options.Applicative
import           Options.Applicative.Types
import           System.Directory
import           System.Environment
import           System.FilePath

import           Git.Vogue
import           Git.Vogue.Types

import           Git.Vogue.PluginCommon
import qualified Paths_git_vogue           as Paths

-- | Parse command-line options.
optionsParser :: Parser VogueOptions
optionsParser = flip Options
    <$> commandParser
    <*> searchP
  where
    searchP :: Parser SearchMode
    searchP = fileList <|> allFlag
    fileList = FindSpecific <$> some (argument str (metavar "FILE"))
    allFlag = flag FindChanged FindAll
        (  long "all"
        <> short 'A'
        <> help "Apply to all files, not just changed files."
        )

commandParser :: Parser VogueCommand
commandParser = subparser
    ( command "init" (info pInit
        (progDesc "Initialise git-vogue support in a git repo"))
    <> pCommand "verify"
                CmdVerify
                "Check git-vogue support is all legit"
    <> pCommand "plugins"
                CmdPlugins
                "List installed plugins."
    <> pCommand "check"
                CmdRunCheck
                "Run check plugins on files in a git repo"
    <> pCommand "fix"
                CmdRunFix
                "Run fix plugins on files a git repo"
    )
  where
    pInit = CmdInit <$> option (Just <$> readerAsk)
        (  long "template"
        <> value Nothing
        )

-- | Discover all available plugins.
--
-- This function inspects the $PREFIX/libexec/git-vogue directory and the
-- directories listed in the $GIT_VOGUE_PATH environmental variable (if
-- defined) and builds a 'Plugin' for the executables found.
discoverPlugins :: IO [Plugin]
discoverPlugins = do
    -- Use the environmental variable and $libexec/git-vogue/ directories as
    -- the search path.
    path <- fromMaybe "" <$> lookupEnv "GIT_VOGUE_PATH"
    libexec <- (</> "git-vogue") <$> Paths.getLibexecDir
    let directories = splitOn ":" path <> [libexec]

    -- Find all executables in the directories in path.
    plugins <- (fmap . fmap) fromString
                  (traverse ls directories >>= filterM isExecutable . concat)

    -- Filter out disabled plugins.
    disabled_plugins <- disabledPlugins
    return . filter (not . pluginIn disabled_plugins) $ plugins
  where
    ls :: FilePath -> IO [FilePath]
    ls p = do
        exists <- doesDirectoryExist p
        if exists
            then fmap (p </>) <$> getDirectoryContents p
            else return []

    isExecutable :: FilePath -> IO Bool
    isExecutable = fmap executable . getPermissions

    pluginIn :: [String] -> Plugin -> Bool
    pluginIn disabled_plugins p =
        (takeBaseName . unPlugin $ p) `elem` disabled_plugins

-- | Parse the command line and run the command.
main :: IO ()
main = do
  opt <- execParser opts
  plugins <- discoverPlugins
  runVogue plugins (runCommand (optCommand opt) (optSearch opt))
  where
    opts = info (helper <*> optionsParser)
      ( fullDesc
     <> progDesc "Make your Haskell git repository fashionable"
     <> header "git-vogue - git integration for fashionable Haskell" )
