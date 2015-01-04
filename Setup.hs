module Main where

import           Control.Monad
import           Data.List
import           Distribution.PackageDescription    (PackageDescription (..))
import           Distribution.Simple
import           Distribution.Simple.Install
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils
import           System.FilePath

-- | Run cabal with custom copyHook which puts our extra executables in the
-- libexec directory.
main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
    { copyHook = copyThings
    }

-- | Copy "extra" executables to $PREFIX/libexec/git-vogue/ instead of
-- $PREFIX/bin.
copyThings
    :: PackageDescription
    -> LocalBuildInfo
    -> UserHooks
    -> CopyFlags
    -> IO ()
copyThings pkg lbi _ flags = do
    -- First install only the "main" components with the default settings.
    let (main_pkg, main_lbi) = tweakMainInstall pkg lbi
    install main_pkg main_lbi flags

    -- Then install the "secondary" executables and plugin scripts with the
    -- special settings.
    let (sub_pkg, sub_lbi) = tweakSubcommandInstall pkg lbi
    install sub_pkg sub_lbi flags

    -- Install shell scripts from plugins/.
    --
    -- This was stolen from "Distribution.Simple.Install" and hacked. Why is
    -- cabal so terrible compared to make and friends?
    let scripts = fmap stripPluginPrefix . filter isPlugin $ dataFiles pkg
        verbosity = fromFlag (copyVerbosity flags)
        src_data_dir = dataDir sub_pkg
        dest_data_dir = datadir . absoluteInstallDirs sub_pkg sub_lbi $
                        fromFlag (copyDest flags)
    forM_ scripts $ \ file -> do
        let dir = takeDirectory file
        files <- matchDirFileGlob src_data_dir file
        createDirectoryIfMissingVerbose verbosity True (dest_data_dir </> dir)
        forM_ files $ \file' ->
            installExecutableFile verbosity (src_data_dir </> file')
                                            (dest_data_dir </> file')

-- | Directory our plugin scripts are kept in.
pluginPrefix :: FilePath
pluginPrefix = "plugins/"

-- | Strip the 'pluginPrefix' off the front of a 'FilePath'.
stripPluginPrefix :: FilePath -> FilePath
stripPluginPrefix = dropWhile (== '/') . dropWhile (/= '/')

-- | A data file is in the plugins directory.
isPlugin :: FilePath -> Bool
isPlugin = (pluginPrefix `isPrefixOf`)

-- | Tweak parameters for install of main components (i.e. library and first
-- executable) only.
tweakMainInstall
    :: PackageDescription
    -> LocalBuildInfo
    -> (PackageDescription, LocalBuildInfo)
tweakMainInstall pkg lbi =
    let pkg' = pkg { executables = take 1 $ executables pkg
                   , dataFiles = filter (not . isPlugin) $ dataFiles pkg
                   }
    in (pkg', lbi)

-- | Tweak parameters for install of secondary components (i.e. executables
-- other than the first.
tweakSubcommandInstall
    :: PackageDescription
    -> LocalBuildInfo
    -> (PackageDescription, LocalBuildInfo)
tweakSubcommandInstall pkg lbi =
    let dest = suffixIt . libexecdir $ installDirTemplates lbi
        pkg' = pkg { executables = tail $ executables pkg
                   , dataFiles = []
                   , dataDir = dataDir pkg </> pluginPrefix
                   , library = Nothing
                   , testSuites = []
                   , benchmarks = []
                   , extraSrcFiles = []
                   , extraTmpFiles = []
                   , extraDocFiles = []
                   }
        lbi' = lbi { installDirTemplates = (installDirTemplates lbi)
                        { bindir = dest
                        , datadir = dest
                        , datasubdir = toPathTemplate ""
                        }
                   }
    in (pkg', lbi')
  where
    suffixIt = toPathTemplate . (</> "git-vogue") . fromPathTemplate
