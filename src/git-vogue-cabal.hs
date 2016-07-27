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

import           Control.Monad                                 (unless, when)
import           Data.Foldable
import           Data.List                                     hiding (and)
import           Data.Monoid
import           Distribution.PackageDescription.Check
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.PackageDescription.Parse         (readPackageDescription)
import           Distribution.Simple.Utils                     (defaultPackageDesc,
                                                                toUTF8,
                                                                wrapText)
import           Distribution.Verbosity                        (silent)
import           Git.Vogue.PluginCommon
import           Prelude                                       hiding (and,
                                                                mapM_)
import           System.Exit

main :: IO ()
main = f =<< getPluginCommand
                "Check your Haskell project for cabal-related problems."
                "git-vogue-cabal - check for cabal problems"
  where
    f CmdName  = putStrLn "cabal"

    f (CmdCheck check_fs_list all_fs_list) = do
        check_fs <- read <$> readFile check_fs_list
        all_fs <- read <$> readFile all_fs_list
        -- Grab all the projects dirs we want to traverse through
        rs <- forProjects (hsProjects check_fs all_fs) (const check)
        unless (and rs) exitFailure

    f CmdFix{} = do
        outputBad $ "There are outstanding cabal failures, you need to fix this "
                <> "manually and then re-run check"
        exitFailure

-- | Runs the same thing as cabal check.
-- See also "Distribution.Client.Check" in cabal-install.
check :: IO Bool
check = do
    pdfile <- defaultPackageDesc silent
    ppd <- readPackageDescription silent pdfile
    let pkg_desc = flattenPackageDescription ppd
    ioChecks <- checkPackageFiles pkg_desc "."
    let packageChecks = filter goodCheck $ ioChecks <> checkPackage ppd (Just pkg_desc)
        buildImpossible = [ x | x@PackageBuildImpossible {} <- packageChecks ]
        buildWarning    = [ x | x@PackageBuildWarning {}    <- packageChecks ]
        distSuspicious  = [ x | x@PackageDistSuspicious{}   <- packageChecks ]
        distInexusable  = [ x | x@PackageDistInexcusable {} <- packageChecks ]
    unless (null buildImpossible) $ do
        outputBad "The package will not build sanely due to these errors:"
        printCheckMessages buildImpossible
    unless (null buildWarning) $ do
        outputBad "The following warnings are likely affect your build negatively:"
        printCheckMessages buildWarning
    unless (null distSuspicious) $ do
        outputBad "These warnings may cause trouble when distributing the package:"
        printCheckMessages distSuspicious
    unless (null distInexusable) $ do
        outputBad "The following errors will cause portability problems on other environments:"
        printCheckMessages distInexusable
    let isDistError PackageDistSuspicious {} = False
        isDistError _                        = True
        errors = filter isDistError packageChecks
    unless (null errors) $
        outputBad "Hackage would reject this package."
    when (null packageChecks) $
        outputGood "Checked cabal file"
    return (null packageChecks)
  where
    goodCheck (PackageDistSuspicious msg) =
        not $ "ghc-options: -O2" `isInfixOf` msg
    goodCheck _ = True

    printCheckMessages = mapM_ (outputBad . format . explanation)
    format = toUTF8 . wrapText . ("* "++)
