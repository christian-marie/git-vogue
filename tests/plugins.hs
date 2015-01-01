--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Description: Test git repository setup.
module Main where

import           Control.Monad.IO.Class ()
import           Data.Monoid
import           System.FilePath
import           Test.Hspec

import           Git.Vogue.Plugins
import           Git.Vogue.Types

failingPluginExecutorImpl :: PluginExecutorImpl IO
failingPluginExecutorImpl =
        PluginExecutorImpl (const . return $ Failure mempty mempty)
                           (const . return $ Failure mempty mempty)

succeedingPluginExecutorImpl :: PluginExecutorImpl IO
succeedingPluginExecutorImpl =
        PluginExecutorImpl (const . return $ Success mempty mempty)
                           (const . return $ Success mempty mempty)

main :: IO ()
main = hspec $ do
    describe "module collation" $ do
        it "maximum of statuses is failure" $
            maximum [Success mempty mempty, Failure mempty mempty]
                `shouldBe` Failure mempty mempty

        it "fails if one module fails" $ do
            checkPlugins failingPluginExecutorImpl ["a", "b"]
              >>= (`shouldBe` Failure mempty "\x1b[33m failed with \x1b[0m\n\x1b[33m failed with \x1b[0m\n")
            fixPlugins failingPluginExecutorImpl ["monkey"]
              >>= (`shouldBe` Failure mempty "\x1b[33m failed with \x1b[0m\n")

        it "succeeds if all modules succeed" $ do
            checkPlugins succeedingPluginExecutorImpl ["a", "b"]
              >>= (`shouldBe` Success mempty "\x1b[32m succeeded with \x1b[0m\n\x1b[32m succeeded with \x1b[0m\n")
            fixPlugins succeedingPluginExecutorImpl ["monkey"]
              >>= (`shouldBe` Success mempty "\x1b[32m succeeded with \x1b[0m\n")

    describe "IO module executor" $ do
        it "check fails on failing module" $
            runCheckExecutor "failing"
                             (Failure "failing" "ohnoes\n")

        it "check fails on succeeding module" $
            runCheckExecutor "succeeding"
                             (Success "succeeding" "yay\n")

        it "check fails on exploding module" $
            runCheckExecutor "exploding"
                             (Catastrophe "exploding" "half-life 3 confirmed\n")

        it "fix fails on failing module" $
            runFixExecutor "failing"
                             (Failure "failing" "ohnoes\n")

        it "fix fails on succeeding module" $
            runFixExecutor "succeeding"
                             (Success "succeeding" "yay\n")

        it "fix fails on exploding module" $
            runFixExecutor "exploding"
                             (Catastrophe "exploding" "half-life 3 confirmed\n")

runCheckExecutor :: FilePath -> Status Check -> Expectation
runCheckExecutor = runTestExecutor executeCheck

runFixExecutor :: FilePath -> Status Fix -> Expectation
runFixExecutor = runTestExecutor executeFix

runTestExecutor
    :: (PluginExecutorImpl IO -> Plugin -> IO (Status a))
    -> FilePath
    -> Status a
    -> Expectation
runTestExecutor act file expected =
    act ioPluginExecutorImpl (Plugin ("fixtures" </> file))
      >>= (`shouldBe` expected)
