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
import           Data.Char
import           Data.Foldable
import           Data.List               hiding (elem)
import           Data.Maybe
import           Data.Monoid
import           Data.Traversable
import           Git.Vogue.PluginCommon
import           Language.Haskell.GhcMod
import           Prelude                 hiding (elem)
import           System.Exit

main :: IO ()
main =
    f =<< getPluginCommand
            "Check your Haskell project for ghc-mod problems."
            "git-vogue-ghc-mod - check for ghc-mod problems"
  where
    f CmdName  = putStrLn "ghc-mod"
    f CmdCheck = ghcModCheck
    f CmdFix   = do
        putStrLn $ "There are outstanding ghc-mod failures, you need to fix this "
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
    | otherwise = s

-- | ghc-mod check all of the .hs files from stdin
ghcModCheck ::  IO ()
ghcModCheck = do
    -- HLint.hs is weird and more of a config file than a source file, so
    -- ghc-mod doesn't like it.
    --
    -- Setup.hs can import cabal things magically, without requiring it to be
    -- mentioned in cabal (which ghc-mod hates)
    --
    -- This is ugly, and should be replaced in favor of a git-vogue ignoring
    -- mechanism.
    files <- filter (not . (`elem` ["HLint.hs", "Setup.hs"])) <$> hsFiles

    -- We can't actually check all at once, or ghc-mod gets confused, so we
    -- traverse
    (r,_) <- runGhcModT defaultOptions (traverse (check . return) files)

    -- Seriously guys? Eithers within eithers?
    warn_errs <- case r of
            -- This is some kind of outer-error, we don't fail on it.
            Left e -> do
                print e
                return []
            -- And these are the warnings and errors.
            Right rs ->
                return rs

    -- Traverse the errors, picking the warnings out. We don't fail on errors
    -- but do warn about them.
    maybe_ws <- for warn_errs $ \warn_err ->
        case warn_err of
            -- Errors in files
            Left e -> do
                putStrLn (explain e)
                return Nothing
            -- Warnings, sometimes empty strings
            Right warn ->
                return $ if null warn then Nothing else Just warn

    let warns = catMaybes maybe_ws
    if null warns
        then do
            putStrLn $ "Checked " <> show (length files)  <> " file(s)"
            exitSuccess
        else do
            traverse_ putStrLn warns
            exitFailure
