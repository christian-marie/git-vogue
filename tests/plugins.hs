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

failingExecutor, succeedingExecutor :: b -> IO (Status a)
failingExecutor    = const . return $ Failure mempty mempty
succeedingExecutor = const . return $ Success mempty mempty

main :: IO ()
main = hspec $ do
    describe "module collation" $ do
        it "maximum of statuses is failure" $
            maximum [Success mempty mempty, Failure mempty mempty]
                `shouldBe` Failure mempty mempty

        it "fails if one module fails" $ do
            getWorst failingExecutor ["a", "b"]
              >>= (`shouldBe` Failure mempty "\x1b[33m failed with:\n\x1b[0m\n\x1b[33m failed with:\n\x1b[0m\n")
            getWorst failingExecutor  ["monkey"]
              >>= (`shouldBe` Failure mempty "\x1b[33m failed with:\n\x1b[0m\n")

        it "succeeds if all modules succeed" $ do
            getWorst succeedingExecutor ["a", "b"]
              >>= (`shouldBe` Success mempty "\x1b[32m succeeded with:\n\x1b[0m\n\x1b[32m succeeded with:\n\x1b[0m\n")
            getWorst succeedingExecutor ["monkey"]
              >>= (`shouldBe` Success mempty "\x1b[32m succeeded with:\n\x1b[0m\n")

    describe "IO module executor" $ do
        describe "check" $ do
            it "fails on failing module" $
                runCheckExecutor FindAll "failing"
                                (Failure "failing" "ohnoes\n")

            it "fails on succeeding module" $
                runCheckExecutor FindAll "succeeding"
                                (Success "succeeding" "yay\n")

            it "fails on exploding module" $
                runCheckExecutor FindAll "exploding"
                                (Catastrophe 3 "exploding" "something broke\n")

        describe "fix" $ do
            it "fails on failing module" $
                runFixExecutor FindAll "failing"
                                (Failure "failing" "ohnoes\n")

            it "fails on succeeding module" $
                runFixExecutor FindAll "succeeding"
                                (Success "succeeding" "yay\n")

            it "fails on exploding module" $
                runFixExecutor FindAll "exploding"
                                (Catastrophe 3 "exploding" "something broke\n")

runCheckExecutor :: SearchMode -> FilePath -> Status Check -> Expectation
runCheckExecutor = runTestExecutor executeCheck

runFixExecutor :: SearchMode -> FilePath -> Status Fix -> Expectation
runFixExecutor = runTestExecutor executeFix

runTestExecutor
    :: (PluginExecutorImpl IO -> SearchMode -> Plugin -> IO (Status a))
    -> SearchMode
    -> FilePath
    -> Status a
    -> Expectation
runTestExecutor act search_mode file expected =
    act ioPluginExecutorImpl search_mode (Plugin ("fixtures" </> file))
      >>= (`shouldBe` expected)
