{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Git.Vogue.Modules where

import           Git.Vogue.Types

import           Data.Monoid
import           Data.String
import           Data.String.Utils
import           Data.Text         (Text)
import qualified Data.Text         as T
import           System.Exit
import           System.Process

ioModuleExecutorImpl :: ModuleExecutorImpl IO
ioModuleExecutorImpl =
    ModuleExecutorImpl (f "fix") (f "check")
  where
    f arg (ModulePath path) = do
        name <- getName path
        (status, stdout, stderr) <- readProcessWithExitCode path [arg] mempty
        let glommed = fromString $ stdout <> stderr
        return $ case status of
            ExitSuccess -> Success name glommed
            ExitFailure 1 -> Failure name glommed
            ExitFailure _ -> Catastrophe name glommed

    getName path = do
        (status, name, _) <- readProcessWithExitCode path ["name"] mempty
        return . ModuleName . fromString . strip $ case status of
            ExitSuccess -> if null name then path else name
            ExitFailure _ -> path

colorize :: Status a -> Text
colorize (Success     (ModuleName x) y) = "\x1b[32m" <> x <> " succeeded with " <> y <> "\x1b[0m"
colorize (Failure     (ModuleName x) y) = "\x1b[33m" <> x <> " failed with "    <> y <> "\x1b[0m"
colorize (Catastrophe (ModuleName x) y) = "\x1b[31m" <> x <> " exploded with "  <> y <> "\x1b[0m"

checkModules
    :: Monad m
    => ModuleExecutorImpl m
    -> [ModulePath]
    -> m (Status Check)
checkModules ModuleExecutorImpl{..} ps = do
    rs <- mapM executeCheck ps
    return $ insertMax rs (T.unlines $ map colorize rs)

fixModules
    :: Monad m
    => ModuleExecutorImpl m
    -> [ModulePath]
    -> m (Status Fix)
fixModules ModuleExecutorImpl{..} ps = do
    rs <- mapM executeFix ps
    return $ insertMax rs (T.unlines $ map colorize rs)

insertMax :: [Status a] -> Text -> Status a
insertMax rs txt =
    case maximum rs of
        Success{} -> Success mempty txt
        Failure{} -> Failure mempty txt
        Catastrophe{} -> Catastrophe mempty txt
