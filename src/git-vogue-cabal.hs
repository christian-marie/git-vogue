-- | Description: Check with "cabal check".
module Main where

import           Common
import           Control.Monad                                 (unless, when)
import           Data.Monoid
import           Distribution.PackageDescription.Check
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.PackageDescription.Parse         (readPackageDescription)
import           Distribution.Simple.Utils                     (defaultPackageDesc,
                                                                toUTF8,
                                                                wrapText)
import           Distribution.Verbosity                        (Verbosity,
                                                                silent)
import           System.Exit

main :: IO ()
main = f =<< getPluginCommand
                "Check your Haskell project for cabal-related problems."
                "git-vogue-cabal - check for cabal problems"
  where
    f CmdName  = putStrLn "cabal"
    f CmdCheck = do
        ok <- check silent
        unless ok exitFailure
    f CmdFix     = exitFailure

-- | Runs the same thing as cabal check.
-- See also "Distribution.Client.Check" in cabal-install.
check :: Verbosity -> IO Bool
check verbosity = do
    pdfile <- defaultPackageDesc verbosity
    ppd <- readPackageDescription verbosity pdfile
    let pkg_desc = flattenPackageDescription ppd
    ioChecks <- checkPackageFiles pkg_desc "."
    let packageChecks = ioChecks <> checkPackage ppd (Just pkg_desc)
        buildImpossible = [ x | x@PackageBuildImpossible {} <- packageChecks ]
        buildWarning    = [ x | x@PackageBuildWarning {}    <- packageChecks ]
        distSuspicious  = [ x | x@PackageDistSuspicious {}  <- packageChecks ]
        distInexusable  = [ x | x@PackageDistInexcusable {} <- packageChecks ]
    unless (null buildImpossible) $ do
        putStrLn "The package will not build sanely due to these errors:"
        printCheckMessages buildImpossible
    unless (null buildWarning) $ do
        putStrLn "The following warnings are likely affect your build negatively:"
        printCheckMessages buildWarning
    unless (null distSuspicious) $ do
        putStrLn "These warnings may cause trouble when distributing the package:"
        printCheckMessages distSuspicious
    unless (null distInexusable) $ do
        putStrLn "The following errors will cause portability problems on other environments:"
        printCheckMessages distInexusable
    let isDistError (PackageDistSuspicious {}) = False
        isDistError _                          = True
        errors = filter isDistError packageChecks
    unless (null errors) $
        putStrLn "Hackage would reject this package."
    when (null packageChecks) $
        putStrLn "Checked cabal file"
    return (null packageChecks)
  where
    printCheckMessages = mapM_ (putStrLn . format . explanation)
    format = toUTF8 . wrapText . ("* "++)
