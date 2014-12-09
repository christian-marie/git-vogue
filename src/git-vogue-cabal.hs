-- | Description: Check with "cabal check".
module Main where

import           Control.Applicative
import           Data.Monoid
import           Options.Applicative
import           System.Exit
import           System.Process

data Command
    -- | Check the project for problems.
    = CmdCheck
    -- | Fix problems in the project.
    | CmdFix
    -- | Report details.
    | CmdName

execute
    :: Command
    -> IO ()
execute cmd = case cmd of
    CmdName  -> putStrLn "cabal"
    CmdCheck -> callProcess "cabal" ["check"]
    CmdFix   -> exitFailure

optionsParser :: Parser Command
optionsParser = subparser
    (  command "name" (info pName mempty)
    <> command "check" (info pCheck mempty)
    <> command "fix" (info pFix mempty)
    )
  where
    pName = pure CmdName
    pCheck = pure CmdCheck
    pFix = pure CmdFix

main :: IO ()
main = execParser opts >>= execute
  where
    opts = info (helper <*> optionsParser)
        ( fullDesc
        <> progDesc "Check your Haskell project for cabal-related problems."
        <> header "git-vogue-cabal - check for cabal problems" )
