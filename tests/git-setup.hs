{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Description: Test git repository setup.
module Main where

import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.IO.Class      ()
import           Control.Monad.Trans.Control
import           Data.List
import           System.FilePath
import           System.Posix.Files
import           System.Posix.Temp
import           System.Process
import           Test.Hspec

import           Git.Vogue

main :: IO ()
main = hspec . describe "Git repository setup" $ do
    it "should install a new pre-commit hook" $
        withGitRepo $ \path -> do
            let hook = path </> ".git" </> "hooks" </> "pre-commit"
            checkPreCommitHook hook

    it "should update an existing pre-commit hook" $
        withGitRepo $ \path -> do
            let hook = path </> ".git" </> "hooks" </> "pre-commit"
            checkPreCommitHook hook

-- | Check that a pre-commit hook script is "correct".
checkPreCommitHook
    :: FilePath
    -> IO ()
checkPreCommitHook hook = do
    -- Check the hook exists.
    exists <- fileExist hook
    unless exists $ error "Commit hook missing"

    -- Check it has our command in it.
    content <- readFile hook
    unless (preCommitCommand `isInfixOf` content) $
        error "Commit hook does not contain command"

-- | Create a git repository and run an action with it.
withGitRepo
    :: MonadBaseControl IO m
    => (FilePath -> m ())
    -> m ()
withGitRepo action =
    bracket createRepo
            deleteRepo
            action
  where
    createRepo = do
        path <- liftBase $ mkdtemp "/tmp/git-setup-test."
        liftBase $ callProcess "git" ["init", path]
        return path
    deleteRepo path =
        liftBase $ callProcess "rm" ["-rf", path]
