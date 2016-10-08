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

import           Data.String                        (fromString)
import qualified Data.Text.Lazy                     as T
import           Git.Vogue.PluginDiscoverer.Libexec
import           Git.Vogue.VCS.Git
import           Git.Vogue.VCS.Null
import           Options.Applicative

import           Git.Vogue
import           Git.Vogue.Types

import           Git.Vogue.PluginCommon
import           Paths_git_vogue

-- | Parse command-line options.
optionsParser :: Parser VogueOptions
optionsParser = flip Options
    <$> commandParser
    <*> searchP
    <*> many disableP
    <*> vcsP
  where
    searchP :: Parser SearchMode
    searchP = fileList <|> allFlag
    fileList = FindSpecific <$> some (argument str (metavar "FILE"))
    allFlag = flag FindChanged FindAll
        (  long "all"
        <> short 'A'
        <> help "Apply to all files, not just changed files."
        )
    vcsP :: Parser VCSType
    vcsP = flag Git Null
        (  long "nogit"
        <> help "Don't require a .git directory. Some functionality is disabled"
        )


    disableP :: Parser PluginName
    disableP = fromString <$> strOption
        (  long "disable"
        <> short 'd'
        <> help "Disable the plugin"
        <> metavar "PLUGIN_NAME"
        )

commandParser :: Parser VogueCommand
commandParser = subparser
    (  pureSubCommand  "init"
                 CmdInit
                 "Initialise git-vogue support in a git repo"
    <> pureSubCommand "verify"
                CmdVerify
                "Check git-vogue support is all legit"
    <> pureSubCommand "plugins"
                CmdPlugins
                "List installed plugins."
    <> command  "disable" (info (parseEnableDisable CmdDisable)
                                (progDesc "Disable a plugin"))
    <> command  "enable"  (info (parseEnableDisable CmdEnable)
                                (progDesc "Enable a plugin"))
    <> pureSubCommand "check"
                CmdRunCheck
                "Run check plugins on files in a git repo"
    <> pureSubCommand "fix"
                CmdRunFix
                "Run fix plugins on files a git repo"
    )

parseEnableDisable :: (PluginName -> VogueCommand) -> Parser VogueCommand
parseEnableDisable ctor = ctor <$> argument (PluginName . T.pack <$> str)
                                            (metavar "PLUGIN")

-- | Parse the command line and run the command.
main :: IO ()
main = do
  opt <- execParser opts
  let vcs = case optVCS opt of
              Git  -> gitVCS
              Null -> nullVCS
  libexec_path <- getLibexecDir
  runCommand (optCommand opt)
             (optSearch opt)
             (optDisable opt)
             vcs
             (libExecDiscoverer libexec_path)
  where
    opts = info (helper <*> optionsParser)
      ( fullDesc
     <> progDesc "Make your Haskell git repository fashionable"
     <> header "git-vogue - git integration for fashionable Haskell" )
