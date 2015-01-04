{-# LANGUAGE RecordWildCards #-}

-- | Description: Check with "cabal check".
module Main where

import           Common
import           Data.List
import           Data.Monoid
import           Data.Traversable
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.HLint3
import           System.Directory
import           System.Exit
import           System.FilePath

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
    parsed <- traverse (\f -> parseModuleEx flags f Nothing) files

    let ideas = applyHints classify hint [ x | Right x <- parsed]
    let errors = [ parseErrorMessage x
                <> show (parseErrorLocation x) | Left x <- parsed ]
    let out = unlines errors <> "\n" <> (ideas >>= showANSI)

    if null ideas && null errors
      then do
        putStrLn ("checked " <> show (length files) <> " files")
        exitSuccess
      else putStrLn out >> exitFailure

-- | The default autoSettings form HLint3 does not handle custom HLint.hs files
-- in the current directory. So we define our own.
autoSettings' :: IO (ParseFlags, [Classify], Hint)
autoSettings' = do
    (fixities, classify, hints) <- findSettings readSettingsFile' Nothing
    return (parseFlagsAddFixities fixities defaultParseFlags, classify, resolveHints hints)

-- | This is used to request the location of modules.
--
-- If the module starts with Hlint., we try the current directory, then fall
-- back to the hlint data directory.
--
-- Without this, using a custom HLint.hs does not work.
readSettingsFile' :: String -> IO (FilePath, Maybe String)
readSettingsFile' x
    | Just suffix <- "HLint." `stripPrefix` x = do
        let file = suffix <.> "hs"
        relative <- doesFileExist file
        if relative
            then return (file, Nothing)
            else do
                dir <- getHLintDataDir
                return (dir </> file, Nothing)
    | otherwise = return (x <.> "hs", Nothing)

-- All of the code below is more or less salvaged from hlint internals.

-- | Pretty print and Idea with colouring
showANSI :: Idea -> String
showANSI i =
    showEx (\x -> "\x1b[36m" <> x <> "\x1b[0m") i <> "\n"

-- | Show an idea with a function that highlights.
showEx :: (String -> String) -> Idea -> String
showEx tt Idea{..} = unlines $
    ["\x1b[35m" <> showSrcLoc (getPointLoc ideaSpan) <> "\x1b[0m " <> (if ideaHint == "" then "" else show ideaSeverity <> ": " <> ideaHint)] <>
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


