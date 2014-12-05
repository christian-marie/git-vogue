{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Git.Vogue.Modules where

import           Git.Vogue.Types

import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.String
import           Data.String.Utils
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           System.Exit
import           System.Process

ioModuleExecutorImpl :: ModuleExecutorImpl IO
ioModuleExecutorImpl =
    ModuleExecutorImpl (f "fix") (f "check")
  where
    f arg (Plugin path) = do
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

checkModules'
    :: MonadIO m
    => [Plugin]
    -> m ()
checkModules' ps = liftIO $ do
    st <- fixModules ioModuleExecutorImpl ps
    case st of
        Success{} ->
            exitSuccess
        Failure _ output -> do
            T.putStrLn output
            exitWith $ ExitFailure 1
        Catastrophe _ output -> do
            T.putStrLn output
            exitWith $ ExitFailure 2

fixModules'
    :: MonadIO m
    => [Plugin]
    -> m ()
fixModules' ps = liftIO $ do
    st <- checkModules ioModuleExecutorImpl ps
    case st of
        Success _ output -> do
            T.putStrLn output
            exitSuccess
        Failure _ output -> do
            T.putStrLn output
            exitWith $ ExitFailure 1
        Catastrophe _ output -> do
            T.putStrLn output
            exitWith $ ExitFailure 2

checkModules
    :: Monad m
    => ModuleExecutorImpl m
    -> [Plugin]
    -> m (Status Check)
checkModules ModuleExecutorImpl{..} ps = do
    rs <- mapM executeCheck ps
    return $ insertMax rs (T.unlines $ map colorize rs)

fixModules
    :: Monad m
    => ModuleExecutorImpl m
    -> [Plugin]
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
