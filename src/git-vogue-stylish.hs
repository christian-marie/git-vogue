-- | Description: Check with "cabal check".
module Main where

import           Common
import           Control.Applicative
import           Data.Algorithm.Diff
import           Data.Algorithm.DiffOutput
import           Data.List
import           Data.Monoid               hiding (First)
import           Language.Haskell.Stylish
import           System.Exit
import           System.IO                 hiding (hGetContents)
import           System.IO.Strict          (hGetContents)

main :: IO ()
main =
    f =<< getPluginCommand
            "Check your Haskell project for stylish-haskell-related problems."
            "git-vogue-stylish - check for stylish-haskell problems"
  where
    f CmdName  = putStrLn "stylish"
    f CmdCheck = run stylishCheckFile succeedIfAll
    f CmdFix   = run stylishRunFile (const exitSuccess)

--------------------------------------------------------------------------------
-- | Run a stylish operation and do something depending on the result
run
    :: Show a => (FilePath -> Config -> IO a) -- ^ Runs Stylish and gets a result
    -> ([a] -> IO b) -- ^ Transforms a list of results into an exit code
    -> IO b -- ^ Exit code
run fn op = do
    files <- filter (isSuffixOf ".hs") . lines <$> getContents
    cfg <- getConfig
    results <- mapM (`fn` cfg) files
    op results

-- | Exit with success if and only if all results are valid
succeedIfAll
    :: [Bool] -- ^ List of results
    -> IO b -- ^ Exit code
succeedIfAll rs =
    if and rs
        then do
            putStrLn $ "Checked " <> show (length rs) <> " files"
            exitSuccess
        else
            exitFailure

--------------------------------------------------------------------------------
-- | Gets the default configuration at `data/stylish-haskell.yaml`.
getConfig
    :: IO Config -- ^ Stylish Haskell config
getConfig =
    defaultConfigFilePath >>= loadConfig (makeVerbose False) . Just

-- | Checks whether running Stylish over a given file produces any differences.
-- Returns TRUE if there's nothing left to change.
-- Prints results and returns FALSE if there are things left to change.
stylishCheckFile
    :: FilePath -- ^ File to run through Stylish
    -> Config -- ^ Stylish Haskell config
    -> IO Bool
stylishCheckFile fp cfg = stylishFile fp cfg (\original stylish ->
    case getStyleDiffs original stylish of
        [] -> return True
        x  -> do
            putStrLn $ "Some things need fixing in " ++ fp
            putStrLn $ ppDiff x
            return False
    )

-- | Runs Stylish over a given file. If there are changes, write them.
stylishRunFile
    :: FilePath -- ^ File to run through Stylish
    -> Config -- ^ Stylish Haskell config
    -> IO ()
stylishRunFile fp cfg = stylishFile fp cfg (\original stylish ->
    case getStyleDiffs original stylish of
        [] -> return ()
        _  -> do
            writeFile fp stylish
            return ()
    )

--------------------------------------------------------------------------------
-- | Get diffs and filter out equivalent ones
getStyleDiffs
    :: String
    -> String
    -> [Diff [String]]
getStyleDiffs original stylish = filter filterDiff $
    getGroupedDiff (lines original) (lines stylish)

-- | Filters out equal diffs
filterDiff
    :: (Eq a)
    => Diff a
    -> Bool
filterDiff (Both a b) = a /= b
filterDiff _          = True

--------------------------------------------------------------------------------
-- | Takes an original file, the Stylish version of the file, and does something
-- depending on the outcome of the Stylish transformation.
stylishFile
    :: FilePath -- ^ File to run through Stylish
    -> Config -- ^ Stylish Haskell config
    -> (String -> String -> IO a)
    -> IO a
stylishFile fp cfg fn = do
    original <- readUTF8File fp
    stylish  <- stylishFile' (Just fp) cfg
    fn original stylish

-- | Processes a single file, or stdin if no filepath is given
stylishFile'
    :: Maybe FilePath -- ^ File to run through Stylish
    -> Config -- ^ Stylish Haskell config
    -> IO String
stylishFile' mfp conf = do
    contents <- maybe getContents readUTF8File mfp
    let result = runSteps (configLanguageExtensions conf)
            mfp (configSteps conf) $ lines contents
    case result of
        Left  err -> do
            hPutStrLn stderr err
            return contents
        Right ok  -> return $ unlines ok

-- | Loads a UTF8 file.
readUTF8File
    :: FilePath -- ^ Filepath to read
    -> IO String -- ^ File data
readUTF8File fp =
     withFile fp ReadMode $ \h -> do
        hSetEncoding h utf8
        hGetContents h
