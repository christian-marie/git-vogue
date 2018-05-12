--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE ViewPatterns #-}

-- | Description: Check with "packunused".
module Main where

import           Control.Exception
import           Control.Monad          (unless)
import           Data.Foldable
import           Data.Monoid
import           Git.Vogue.PluginCommon
import           Prelude                hiding (and)
import           System.Exit
import           System.IO.Error
import           System.Process

main :: IO ()
main = f =<< getPluginCommand
                "Check your Haskell project for redundant dependencies."
                "git-vogue-packunused - check for redundant dependencies"
  where
    f CmdName  = putStrLn "packunused"

    f (CmdCheck check_fs_list all_fs_list) = do
        check_fs <- read <$> readFile check_fs_list
        all_fs <- read <$> readFile all_fs_list
        -- Grab all the projects dirs we want to traverse through
        rs <- forProjects (hsProjects check_fs all_fs) (const check)
        unless (and rs) exitFailure

    f CmdFix{} = do
        outputBad $ "There are redundant dependencies, you need to fix these "
                <> "manually and then re-run check"
        exitFailure

-- | Runs the @packunused@ command, assuming that the build is current.
check :: IO Bool
check = do
    r <- try $ readProcessWithExitCode "packunused"
        [ "--ignore-empty-imports"
        , "--ignore-package", "base"
        ] ""
    case r of
        Right (ExitSuccess, _, _) -> do
            outputGood "No unused dependencies. "
            return True
        Right (ExitFailure e, sout, _serr) -> do
            outputBad sout
            return False
        Left (isDoesNotExistError -> True) -> do
            outputUnfortunate "Could not find packunused, vacuously succeeding."
            return True
        Left e -> throwIO e
