-- | Description: Check and fix style differences with stylish-haskell
module Main where

import           Common
import           Control.Monad
import           Data.Algorithm.Diff
import           Data.Algorithm.DiffOutput
import           Data.Foldable
import           Data.List                 hiding (and)
import           Data.Monoid               hiding (First)
import           Data.Traversable
import           Language.Haskell.Stylish
import           Prelude                   hiding (and)
import           System.Exit
import           System.IO                 hiding (hGetContents)
import           System.IO.Strict          (hGetContents)

main :: IO ()
main = do
    cmd <- getPluginCommand
            "Check your Haskell project for stylish-haskell-related problems."
            "git-vogue-stylish - check for stylish-haskell problems"
    cfg <- getConfig
    files <- hsFiles
    f files cfg cmd
  where
    f _ _ CmdName  = putStrLn "stylish"
    f files cfg CmdCheck = do
        rs <- traverse (stylishCheckFile cfg) files
        if and rs
            then do
                putStrLn $ "Checked " <> show (length rs) <> " file(s)"
                exitSuccess
            else
                exitFailure

    f files cfg CmdFix = do
        -- Fix all of the things first
        traverse_ (stylishRunFile cfg) files
        -- Now double check they are fixed
        rs <- traverse (stylishCheckFile cfg) files
        if and rs
            then
                putStrLn "Style converged"
            else do
                putStrLn "Style did not converge, bailing"
                exitFailure

-- | Try various configuration locations as per stylish-haskell binary
getConfig
    :: IO Config
getConfig =
    -- Don't spew every file checked to stdout
    let v = makeVerbose False
    in configFilePath v Nothing >>= loadConfig v

-- | Checks whether running Stylish over a given file produces any differences.
-- Returns TRUE if there's nothing left to change.
-- Prints results and returns FALSE if there are things left to change.
stylishCheckFile
    :: Config -- ^ Stylish Haskell config
    -> FilePath -- ^ File to run through Stylish
    -> IO Bool
stylishCheckFile cfg fp = stylishFile fp cfg (\original stylish ->
    case getStyleDiffs original stylish of
        [] -> return True
        x  -> do
            putStrLn $ "\x1b[33m" <> fp <> "\x1b[0m"
                    <> " has differing style:\n" <> ppDiff x
            return False
    )

-- | Runs Stylish over a given file. If there are changes, write them.
stylishRunFile
    :: Config -- ^ Stylish Haskell config
    -> FilePath -- ^ File to run through Stylish
    -> IO ()
stylishRunFile cfg fp = stylishFile fp cfg $ \original stylish ->
    unless (null $ getStyleDiffs original stylish) (writeFile fp stylish)

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
