--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

-- | Description: Test git repository setup.
module Main where

import           Control.Monad
import           System.Directory
import           System.IO.Temp
import           Test.Hspec

import           Git.Vogue.Types
import           Git.Vogue.VCS.Git

main :: IO ()
main = test gitVCS

test :: VCS IO -> IO ()
test VCS{..} =
    hspec . describe "Git VCS implementation" $ do
        it "should install and remove a pre-commit hook" . withGitRepo $ do
            checkHook >>= (`shouldBe` False)
            installHook
            checkHook >>= (`shouldBe` True)
            removeHook
            checkHook >>= (`shouldBe` False)

        it "should list files correctly"  . withGitRepo $ do
            getFiles FindChanged >>= (`shouldBe` [])
            getFiles FindAll     >>= (`shouldBe` [])

            writeFile "hi" "there"
            getFiles FindChanged >>= (`shouldBe` [])
            getFiles FindAll     >>= (`shouldBe` [])

            void $ git ["add", "hi"]
            getFiles FindChanged >>= (`shouldBe` ["hi"])
            getFiles FindAll     >>= (`shouldBe` ["hi"])

            void $ git ["commit", "-m", "add hi", "hi"]
            getFiles FindChanged >>= (`shouldBe` [])
            getFiles FindAll     >>= (`shouldBe` ["hi"])

-- | Create a git repository and run an action with it, after changing to that
-- directory.
--
-- Restores current dir on completion
withGitRepo
    :: IO ()
    -> IO ()
withGitRepo f =
    withSystemTempDirectory "git-setup-test." $ \temp_dir -> do
        -- For some unknown reason, setting the current directory appears to do
        -- strange things with a bracket, so we don't bracket.
        before_dir <- getCurrentDirectory
        void $ git ["init", temp_dir]
        setCurrentDirectory temp_dir
        f
        setCurrentDirectory before_dir

