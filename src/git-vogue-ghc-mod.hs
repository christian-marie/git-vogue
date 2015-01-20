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
import           Data.Functor
import           Data.List               hiding (notElem)
import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Maybe
import           Data.Monoid
import           Data.Traversable
import           Git.Vogue.PluginCommon
import           Language.Haskell.GhcMod
import           Prelude                 hiding (notElem)
import           System.Directory
import           System.Exit
import           System.FilePath

main :: IO ()
main =
    f =<< getPluginCommand
            "Check your Haskell project for ghc-mod problems."
            "git-vogue-ghc-mod - check for ghc-mod problems"
  where
    forWithKey_ :: Applicative f => Map k v -> (k -> v -> f ()) -> f ()
    forWithKey_ m a = void $ M.traverseWithKey a m
    f CmdName  = putStrLn "ghc-mod"
    f (CmdCheck check_fs all_fs) = do
        cwd <- getCurrentDirectory  >>= canonicalizePath
        -- Have to change to the project directory for each ghc-mod run or it
        -- will be sad.
        --
        -- We run ghcModCheck in each, which will exit on the first failure.
        forWithKey_ (hsProjects check_fs all_fs) $ \dir fs -> do
            let pdir = "." </> dir
            putStrLn $ "Checking " <> pdir
            setCurrentDirectory pdir

            -- HLint.hs is weird and more of a config file than a source file, so
            -- ghc-mod doesn't like it.
            --
            -- Setup.hs can import cabal things magically, without requiring it to
            -- be mentioned in cabal (which ghc-mod hates)
            let rels = catMaybes $ fmap (stripPrefix dir) fs
            ghcModCheck $ filter (\x ->    not ("HLint.hs" `isSuffixOf` x)
                                        && not ("Setup.hs" `isSuffixOf` x)
                                        && ".hs" `isSuffixOf` x) rels
            setCurrentDirectory cwd

        -- If we got this far, there were no failures, so success.
        exitSuccess

    f CmdFix{} = do
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

-- | ghc-mod check all of the given files from the current directory
ghcModCheck :: [FilePath] -> IO ()
ghcModCheck files = do
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
        then
            putStrLn $ "Checked " <> show (length files)  <> " file(s)"
        else do
            traverse_ putStrLn warns
            exitFailure
