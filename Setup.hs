module Main where

import           Data.Monoid
import           Distribution.PackageDescription    (Executable (..),
                                                     PackageDescription (..))
import           Distribution.Simple
import           Distribution.Simple.Install
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
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

    -- Then install the "secondary" components with the special settings.
    let (sub_pkg, sub_lbi) = tweakSubcommandInstall pkg lbi
    install sub_pkg sub_lbi flags

-- | Tweak parameters for install of main components (i.e. library and first
-- executable) only.
tweakMainInstall
    :: PackageDescription
    -> LocalBuildInfo
    -> (PackageDescription, LocalBuildInfo)
tweakMainInstall pkg lbi =
    let pkg' = pkg { executables = take 1 $ executables pkg }
    in (pkg', lbi)

-- | Tweak parameters for install of secondary components (i.e. executables
-- other than the first.
tweakSubcommandInstall
    :: PackageDescription
    -> LocalBuildInfo
    -> (PackageDescription, LocalBuildInfo)
tweakSubcommandInstall pkg lbi =
    let pkg' = pkg { executables = tail $ executables pkg }
        lbi' = lbi { installDirTemplates = (installDirTemplates lbi)
                        { bindir = libexecdir $ installDirTemplates lbi }
                   }
    in (pkg', lbi')
