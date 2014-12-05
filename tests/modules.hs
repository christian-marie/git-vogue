{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Description: Test git repository setup.
module Main where

import           Control.Exception.Lifted
import           Control.Monad.Base
import           Control.Monad.IO.Class      ()
import           Control.Monad.Trans.Control
import           Data.Monoid
import           System.Exit
import           System.FilePath.Posix
import           System.Posix.Temp
import           System.Process
import           Test.Hspec

import           Git.Vogue.Modules
import           Git.Vogue.Types

failingModuleExecutorImpl :: ModuleExecutorImpl IO
failingModuleExecutorImpl =
        ModuleExecutorImpl (const . return $ Failure mempty mempty)
                           (const . return $ Failure mempty mempty)

succeedingModuleExecutorImpl :: ModuleExecutorImpl IO
succeedingModuleExecutorImpl =
        ModuleExecutorImpl (const . return $ Success mempty mempty)
                           (const . return $ Success mempty mempty)

main :: IO ()
main = hspec $ do
    describe "module collation" $ do
        it "maximum of statuses is failure" $
            maximum [Success mempty mempty, Failure mempty mempty]
                `shouldBe` Failure mempty mempty

        it "fails if one module fails" $ do
            checkModules failingModuleExecutorImpl ["a", "b"]
              >>= (`shouldBe` Failure mempty "\x1b[33m failed with \x1b[0m\n\x1b[33m failed with \x1b[0m\n")
            fixModules failingModuleExecutorImpl ["monkey"]
              >>= (`shouldBe` Failure mempty "\x1b[33m failed with \x1b[0m\n")

        it "succeeds if all modules succeed" $ do
            checkModules succeedingModuleExecutorImpl ["a", "b"]
              >>= (`shouldBe` Success mempty "\x1b[32m succeeded with \x1b[0m\n\x1b[32m succeeded with \x1b[0m\n")
            fixModules succeedingModuleExecutorImpl ["monkey"]
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
    :: (ModuleExecutorImpl IO -> Plugin -> IO (Status a))
    -> FilePath
    -> Status a
    -> Expectation
runTestExecutor act mod expected =
    act ioModuleExecutorImpl (Plugin ("fixtures" </> mod))
      >>= (`shouldBe` expected)
