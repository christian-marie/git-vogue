{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

-- | Description: Check with "cabal check".
module Main where

import           Common
import           Control.Applicative
import           Data.Bifunctor
import           Data.List
import           Data.Monoid
import           Data.Traversable
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.HLint3
import           Language.Preprocessor.Cpphs
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
    f CmdCheck = lint
    f CmdFix   = putStrLn "you need to fix hlint failures" >> exitFailure

-- | Lint all of the .hs files from stdin
lint ::  IO ()
lint = do
    files <- hsFiles
    (flags, classify, hint) <- autoSettings'

    -- Cpphs is off by default
    let flags' = flags { cppFlags = Cpphs defaultCpphsOptions }

    -- Traverse the files, parsing and processing as we go for efficiency
    parsed <- for files $ \f ->
                process classify hint <$> parseModuleEx flags' f Nothing


    let ideas = concat [ x | Right x <- parsed]
    let errors = [  x | Left x <- parsed ]
    let out = unlines errors <> "\n" <> ideas

    if null ideas && null errors
      then do
        putStrLn ("checked " <> show (length files) <> " files")
        exitSuccess
      else putStrLn out >> exitFailure
  where
    process classify hint =
        bimap f g
      where
        f x = parseErrorMessage x <> show (parseErrorLocation x)
        g x = showANSI $ applyHints classify hint [x]

-- | The default autoSettings form HLint3 does not handle custom HLint.hs files
-- in the current directory. So we define our own.
autoSettings' :: IO (ParseFlags, [Classify], Hint)
autoSettings' = do
    (fixities, classify, hints) <- findSettings (readSettingsFile Nothing)
                                                (Just "HLint")
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

