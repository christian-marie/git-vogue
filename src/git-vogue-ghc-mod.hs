--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: Check with "cabal check".
module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.List                     hiding (and, notElem)
import           Data.Maybe
import           Data.Monoid
import           Data.Traversable
import           Git.Vogue.PluginCommon
import           Language.Haskell.GhcMod
import           Language.Haskell.GhcMod.Monad
import           Prelude                       hiding (and, notElem)
import           System.Exit

main :: IO ()
main =
    f =<< getPluginCommand
            "Check your Haskell project for ghc-mod problems."
            "git-vogue-ghc-mod - check for ghc-mod problems"
  where
    f CmdName  = putStrLn "ghc-mod"
    f (CmdCheck check_fs_list all_fs_list) = do
        check_fs <- read <$> readFile check_fs_list
        all_fs <- read <$> readFile all_fs_list
        -- Have to change to the project directory for each ghc-mod run or it
        -- will be sad.
        --
        -- We run ghcModCheck in each, which will exit on the first failure.
        rs <- forProjects (hsProjects check_fs all_fs) $ \fs ->
            -- HLint.hs is weird and more of a config file than a source file, so
            -- ghc-mod doesn't like it.
            --
            -- Setup.hs can import cabal things magically, without requiring it to
            -- be mentioned in cabal (which ghc-mod hates)
            ghcModCheck $ filter (\x ->    not ("HLint.hs" `isSuffixOf` x)
                                        && not ("Setup.hs" `isSuffixOf` x)
                                        && ".hs" `isSuffixOf` x) fs

        unless (and rs) exitFailure

    f CmdFix{} = do
        outputBad $ "There are outstanding ghc-mod failures, you need to fix this "
                <> "manually and then re-run check"
        exitFailure

-- | Try to help the user out with some munging of error messages
explain :: String -> String
explain s
    -- A terrible heuristic, but it may help some people
    | "test" `isInfixOf` fmap toLower s && "hidden package" `isInfixOf` s =
        s <> "\n\tSuggestion: cabal configure --enable-tests\n"
    | "bench" `isInfixOf` fmap toLower s && "hidden package" `isInfixOf` s =
        s <> "\n\tSuggestion: cabal configure --enable-benchmarks\n"
    | "hGetContents: invalid argument" `isInfixOf` s =
        s <> "\n\tSuggestion: use cabal < 1.22\n"
    | otherwise = s

-- | ghc-mod check all of the given files from the current directory
--
-- This will print out output, and return a bool representing success.
ghcModCheck :: [FilePath] -> IO Bool
ghcModCheck files = do
    -- We can't actually check all at once, or ghc-mod gets confused, so we
    -- traverse
    (r,_) <- runGmOutT defaultOptions $ runGhcModT defaultOptions (traverse (check . pure) files)

    -- Seriously guys? Eithers within eithers?
    warn_errs <- case r of
            -- This is some kind of outer-error, we don't fail on it.
            Left e -> do
                outputUnfortunate . lineWrap 74 $ show e
                return []
            -- And these are the warnings and errors.
            Right rs ->
                return rs

    -- Traverse the errors, picking errors and warnings out
    maybe_ws <- for warn_errs $ \warn_err ->
        case warn_err of
            -- Errors in files
            Left e ->
                return . Just $ explain e
            -- Warnings, sometimes empty strings
            Right warn ->
                return $ if null warn then Nothing else Just warn

    let warns = catMaybes maybe_ws
    if null warns
        then do
            outputGood $ "Checked " <> show (length files)  <> " file(s)"
            return True
        else do
            traverse_ outputBad warns
            return False
