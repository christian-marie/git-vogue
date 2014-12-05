{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Options.Applicative
import           Options.Applicative.Types

import           Git.Vogue

-- | Parse command line options.
optionsParser :: Parser VogueCommand
optionsParser = subparser
    ( command "init" (info pInit
        (progDesc "Initialise git-vogue support in a git repo"))
    <> command "verify" (info pVerify
        (progDesc "Check git-vogue support is all legit"))
    <> command "check" (info pCheck
        (progDesc "Run check plugins on a git repo"))
    <> command "fix" (info pFix
        (progDesc "Run fix plugins on a git repo"))
    )
  where
    pInit = CmdInit <$> option (Just <$> readerAsk) ( long "template" <> value Nothing)
    pVerify = pure CmdVerify
    pCheck = pure CmdRunCheck
    pFix = pure CmdRunFix

-- | Parse the command line and run the command.
main :: IO ()
main = do
  cmd <- execParser opts
  runVogue plugins (runCommand cmd)
  where
    -- FIXME: read plugins from config
    plugins = ["vogue-stylish-haskell"]
    opts = info (helper <*> optionsParser)
      ( fullDesc
     <> progDesc "Make your Haskell git repository fashionable"
     <> header "git-vogue - git integration for fashionable Haskell" )
