--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Description: Test git repository setup.
module Main where

import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.Monoid
import           System.Directory
import           System.Exit
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

            shouldInstallWorking path hook

    it "should skip an already correct pre-commit hook" $
        withGitRepo $ \path -> do
            let hook = path </> ".git" </> "hooks" </> "pre-commit"

            -- Create an existing hook to update.
            copyHookTemplateTo (Just "templates/pre-commit") hook

            shouldInstallWorking path hook

    it "should report a conflict pre-commit hook" $
        withGitRepo $ \path -> do
            let hook = path </> ".git" </> "hooks" </> "pre-commit"

            -- Create an existing hook to update.
            writeFile hook "echo YAY\n"
            setPermissions hook $ emptyPermissions
                { readable = True
                , executable = True
                }

            -- Run the setup program.
            code <- runInRepo path
            code `shouldBe` ExitFailure 1

-- | Run "git-vogue init" and check we wind up with a working pre-commit hook.
shouldInstallWorking :: FilePath -> FilePath -> IO ()
shouldInstallWorking path hook = do
    code <- runInRepo path
    code `shouldBe` ExitSuccess
    checkPreCommitHook hook

-- | Execute the setup command in a git repository.
runInRepo
    :: FilePath
    -> IO ExitCode
runInRepo path = do
    pwd <- getCurrentDirectory
    let exe = pwd </> "dist/build/git-vogue/git-vogue"
    let tpl = pwd </> "templates/pre-commit"
    ps <- spawnCommand $
        "cd " <> path <> " && " <> exe <> " init --template=" <> tpl
    waitForProcess ps

-- | Check that a pre-commit hook script is "correct".
checkPreCommitHook
    :: FilePath
    -> IO ()
checkPreCommitHook hook = do
    -- Check the hook exists.
    exists <- fileExist hook
    unless exists $ error "Commit hook missing"

    -- Check the hook is executable.
    perm <- getPermissions hook
    unless (executable perm) $ error "Commit hook is not executable"

    -- Check it has our command in it.
    content <- readFile hook
    unless ("git-vogue check" `isInfixOf` content) $
        error "Commit hook does not contain command"

-- | Create a git repository and run an action with it.
withGitRepo
    :: (FilePath -> IO ())
    -> IO ()
withGitRepo = bracket createRepo deleteRepo
  where
    createRepo = do
        path <- mkdtemp "/tmp/git-setup-test."
        callProcess "git" ["init", path]
        return path
    deleteRepo path =
        callProcess "rm" ["-rf", path]
