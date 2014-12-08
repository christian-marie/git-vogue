{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Data.String
import           Options.Applicative
import           Options.Applicative.Types
import           System.Environment
import           System.FilePath

import           Git.Vogue
import           Git.Vogue.Types

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
    pInit = CmdInit <$> option (Just <$> readerAsk)
        (  long "template"
        <> value Nothing
        )
    pVerify = pure CmdVerify
    pCheck = pure CmdRunCheck
    pFix = pure CmdRunFix

-- | Find the list of plugin names.
loadPlugins :: IO [Plugin]
loadPlugins = catch config_plugins default_plugins
  where
    config_plugins = map fromString . lines <$> (configFile "vogue.plugins" >>= readFile)
    default_plugins (SomeException _) = return ["git-vogue-stylish-haskell"]

configFile :: FilePath -> IO FilePath
configFile path = do
    home_dir <- getEnv "HOME"
    return $ home_dir </> ".config" </> "git" </> path

-- | Parse the command line and run the command.
main :: IO ()
main = do
  cmd <- execParser opts
  plugins <- loadPlugins
  runVogue plugins (runCommand cmd)
  where
    opts = info (helper <*> optionsParser)
      ( fullDesc
     <> progDesc "Make your Haskell git repository fashionable"
     <> header "git-vogue - git integration for fashionable Haskell" )
