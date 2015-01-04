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

import           Common
import qualified Paths_git_vogue           as Paths

-- | Parse command line options.
optionsParser :: Parser VogueCommand
optionsParser = subparser
    ( command "init" (info pInit
        (progDesc "Initialise git-vogue support in a git repo"))
    <> pCommand "verify"
                CmdVerify
                "Check git-vogue support is all legit"
    <> pCommand "check"
                CmdRunCheckChanged
                "Run check plugins on all files in a git repo"
    <> pCommand "check-all"
                CmdRunCheckAll
                "Run fix plugins on a git repo"
    <> pCommand "fix"
                CmdRunFixChanged
                "Run fix plugins on a git repo"
    <> pCommand "fix-all"
                CmdRunFixAll
                "Run fix plugins on a git repo"
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
    (fmap . fmap) fromString
                  (traverse ls directories >>= filterM isExecutable . concat)
  where
    ls :: FilePath -> IO [FilePath]
    ls p = do
        exists <- doesDirectoryExist p
        if exists
            then fmap (p </>) <$> getDirectoryContents p
            else return []

    isExecutable :: FilePath -> IO Bool
    isExecutable = fmap executable . getPermissions

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
