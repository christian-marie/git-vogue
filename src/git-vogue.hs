{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.List.Split
import           Data.Maybe
import           Data.String
import           Options.Applicative
import           Options.Applicative.Types
import           System.Directory
import           System.Environment
import           System.FilePath

import           Git.Vogue
import           Git.Vogue.Types

import qualified Paths_git_vogue           as Paths

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
    let directories = (splitOn ":" path) ++ [libexec]

    -- Find all executables in the directories in path.
    filterM doesDirectoryExist directories >>=
        mapM ls >>=
        return . concat >>=
        filterM isExecutable >>=
        return . map fromString
  where
    ls :: FilePath -> IO [FilePath]
    ls p = map (p </>) <$> getDirectoryContents p
    isExecutable :: FilePath -> IO Bool
    isExecutable p = executable <$> getPermissions p

-- | Parse the command line and run the command.
main :: IO ()
main = do
  cmd <- execParser opts
  plugins <- discoverPlugins
  runVogue plugins (runCommand cmd)
  where
    opts = info (helper <*> optionsParser)
      ( fullDesc
     <> progDesc "Make your Haskell git repository fashionable"
     <> header "git-vogue - git integration for fashionable Haskell" )
