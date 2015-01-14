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

import           Git.Vogue.VCS.Git
import           Options.Applicative

import           Git.Vogue
import           Git.Vogue.Types

import           Git.Vogue.PluginCommon

-- | Parse command-line options.
optionsParser :: Parser VogueOptions
optionsParser = Options
    <$> flag FindChanged FindAll
        (  long "all"
        <> short 'A'
        <> help "Apply to all files, not just changed files."
        )
    <*> commandParser

commandParser :: Parser VogueCommand
commandParser = subparser
    (  pCommand  "init"
                 CmdInit
                 "Initialise git-vogue support in a git repo"
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

-- | Parse the command line and run the command.
main :: IO ()
main = do
  opt <- execParser opts
  runCommand (optCommand opt) (optSearch opt) gitVCS undefined
  where
    opts = info (helper <*> optionsParser)
      ( fullDesc
     <> progDesc "Make your Haskell git repository fashionable"
     <> header "git-vogue - git integration for fashionable Haskell" )
