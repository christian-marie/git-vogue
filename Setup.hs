module Main where

import           Data.List
import           Data.Monoid
import           Distribution.PackageDescription    (BuildInfo (..),
                                                     Executable (..),
                                                     PackageDescription (..),
                                                     emptyBuildInfo)
import           Distribution.Simple
import           Distribution.Simple.Install
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils
import           System.Directory
import           System.FilePath

-- | Run cabal with custom copyHook which puts our extra executables in the
-- libexec directory.
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
    let scripts = map stripPluginPrefix . filter isPlugin $ dataFiles pkg
        paths = absoluteInstallDirs sub_pkg sub_lbi $ fromFlag (copyDest flags)
        verbosity = fromFlag (copyVerbosity flags)
    flip mapM_ scripts $ \ file -> do
        let srcDataDir = dataDir sub_pkg
        let destDataDir = datadir paths
        files <- matchDirFileGlob srcDataDir file
        let dir = takeDirectory file
        createDirectoryIfMissingVerbose verbosity True (destDataDir </> dir)
        sequence_ [ installExecutableFile verbosity (srcDataDir  </> file')
                                                    (destDataDir </> file')
                  | file' <- files ]


-- | Directory our plugin scripts are kept in.
pluginPrefix :: FilePath
pluginPrefix = "plugins/"

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
