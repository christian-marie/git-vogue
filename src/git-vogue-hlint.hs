--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

-- | Description: Check with "cabal check".
module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.Foldable
import           Data.List                           hiding (and, concat)
import           Data.Monoid
import           Data.Traversable
import           Git.Vogue.PluginCommon
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.HLint3
import           Language.Preprocessor.Cpphs
import           Prelude                             hiding (and, concat)
import           System.Directory
import           System.Exit

#ifndef GPL_SCARES_ME
import           Language.Haskell.HsColour.Colourise
import           Language.Haskell.HsColour.TTY
#endif

main :: IO ()
main =
    f =<< getPluginCommand
            "Check your Haskell project for hlint-related problems."
            "git-vogue-hlint - check for hlint problems"
  where
    f CmdName  = putStrLn "hlint"
    f (CmdCheck check_fs_list all_fs_list) = do
        check_fs <- read <$> readFile check_fs_list
        all_fs <- read <$> readFile all_fs_list
        -- Traverse the files, parsing and processing as we go for efficiency
        rs <- forProjects (hsProjects check_fs all_fs) $ \fs -> do
            let hss = filter (isSuffixOf ".hs") fs

            (flags, classify, hint) <- autoSettings'
            -- Cpphs is off by default
            let flags' = flags { cppFlags = Cpphs defaultCpphsOptions }


            parsed <- for hss $ \file ->
                process classify hint <$> parseModuleEx flags' file Nothing

            let ideas = concat [ x | Right x <- parsed ]
            let errors = [ x | Left x <- parsed ]
            let out = unlines errors <> "\n" <> ideas

            let ok = null ideas && null errors
            unless ok (outputBad out)
            outputGood $ "Checked " <> show (length hss) <> " file(s)"
            return ok

        unless (and rs) exitFailure
      where
        process classify hint =
            bimap (\x -> parseErrorMessage x <> show (parseErrorLocation x))
                  (\x -> showANSI . filter g $ applyHints classify hint [x])
        g x = case ideaSeverity x of Ignore     -> False
                                     Suggestion -> True
                                     Warning    -> True
                                     Error      -> True

    f CmdFix{} = do
        outputBad $ "There are outstanding hlint failures, you need to fix this "
                <> "manually and then re-run check"
        exitFailure
-- | The default autoSettings form HLint3 does not handle custom HLint.hs files
-- in the current directory. So we define our own.
autoSettings' :: IO (ParseFlags, [Classify], Hint)
autoSettings' = do
    local_hlint <- doesFileExist "HLint.hs"
    let start_at = if local_hlint then Just "HLint" else Nothing
    (fixities, classify, hints) <- findSettings (readSettingsFile Nothing)
                                                start_at
    return (parseFlagsAddFixities fixities defaultParseFlags, classify, resolveHints hints)

#ifdef GPL_SCARES_ME
format :: String -> String
format s = "\x1b[36m" <> s <> "\x1b[0m"
#else
format :: String -> String
format = hscolour defaultColourPrefs
#endif

-- All of the code below is more or less salvaged from hlint internals.

-- | Pretty print and Idea with colouring
showANSI :: [Idea] -> String
showANSI =
    (>>= \i -> showEx format i <> "\n")

-- | Show an idea with a function that highlights.
showEx :: (String -> String) -> Idea -> String
showEx tt Idea{..} = unlines $
    ["\x1b[33m" <> showSrcLoc (getPointLoc ideaSpan) <> "\x1b[0m " <> (if ideaHint == "" then "" else show ideaSeverity <> ": " <> ideaHint)] <>
    f "Found" (Just ideaFrom) <> f "Why not" ideaTo <>
    ["Note: " <> n | let n = showNotes ideaNote, n /= ""]
    where
        f _ Nothing = []
        f msg (Just x) | null xs = [msg <> " remove it."]
                       | otherwise = (msg <> ":") : fmap ("  "<>) xs
            where xs = lines $ tt x

showSrcLoc :: SrcLoc -> String
showSrcLoc (SrcLoc file line col) = file <> ":" <> show line <> ":" <> show col <> ":"

showNotes :: [Note] -> String
showNotes = intercalate ", " . fmap show . filter use
    where use ValidInstance{} = False -- Not important enough to tell an end user
          use _ = True

