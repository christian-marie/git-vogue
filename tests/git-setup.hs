{-# LANGUAGE FlexibleContexts #-}
-- | Description: Test git repository setup.
module Main where

import Control.Exception.Lifted
import Control.Monad.IO.Class ()
import Control.Monad.Trans.Control
import Control.Monad.Base
import System.Process
import System.Posix.Temp
import Test.Hspec

main :: IO ()
main = hspec . describe "Git repository setup" $ do
    it "should install a new pre-commit hook" $
        withGitRepo $ \path -> do
            pending

    it "should update an existing pre-commit hook" $
        withGitRepo $ \path -> do
            pending

    it "should run check (but not fix) in the pre-commit hook" $
        withGitRepo $ \path -> do
            pending

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
