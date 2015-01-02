-- | Description: Check with "cabal check".
module Main where

import           Common
import           Data.Monoid
import           Data.Traversable
import           Language.Haskell.HLint3
import           System.Exit

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
    (flags, classify, hint) <- autoSettings
    parsed <- traverse (\f -> parseModuleEx flags f Nothing) files

    let ideas = applyHints classify hint [ x | Right x <- parsed]
    let errors = [ parseErrorMessage x
                <> show (parseErrorLocation x) | Left x <- parsed ]
    let out = unlines errors <> "\n" <>  show ideas

    if null ideas && null errors
      then do
        putStrLn ("checked " <> show (length files) <> " files")
        exitSuccess
      else putStrLn out >> exitFailure
