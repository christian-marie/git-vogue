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
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

-- | Description: Test git repository setup.
module Main where

import           Control.Applicative
import           Control.Monad
import           System.Directory
import           System.FilePath
import           System.IO.Temp
import           System.Process
import           Test.Hspec

import           Git.Vogue.PluginDiscoverer.Libexec
import           Git.Vogue.Types
import           Git.Vogue.VCS.Git

main :: IO ()
main = do
    abs_fixtures <- canonicalizePath "fixtures"
    hspec $ do
        describe "Git VCS implementation" $
            testGitVCS gitVCS
        describe "Libexec plugin discovery" $
            testLEDiscovery abs_fixtures (libExecDiscoverer "./plugins")

testLEDiscovery :: FilePath -> PluginDiscoverer IO -> Spec
testLEDiscovery fixtures PluginDiscoverer{..} = do
    it "discovers plugins in the libexec dir" . withSetup $  do
        ps <- discoverPlugins
        fmap pluginName ps `shouldBe`
            ["(non-executable) ./plugins/git-vogue/non-executable"
            ,"exploding"
            ,"failing"
            ,"succeeding"
            ]
        fmap enabled ps `shouldBe` [False, True, True, True]

    it "disables and re-enables plugins" . withSetup $ do
        disablePlugin "exploding"
        ps <- filter enabled <$> discoverPlugins
        fmap pluginName ps `shouldBe` ["failing", "succeeding"]

        enablePlugin "exploding"
        ps' <- filter enabled <$> discoverPlugins
        fmap pluginName ps' `shouldBe` ["exploding", "failing", "succeeding"]


    it "provides check methods that do the expected things" . withSetup $ do
        ps <- filter enabled <$> discoverPlugins
        rs <- sequence $ fmap (\Plugin{..} -> runCheck ["magic_file"]) ps
        rs `shouldBe` [ Catastrophe 3 "something broke\n"
                      , Failure "ohnoes\n"
                      , Success "yay\n"]

    it "provides fix methods that do the expected things" . withSetup $ do
        ps <- filter enabled <$> discoverPlugins
        rs <- sequence $ fmap (\Plugin{..} -> runFix ["magic_file"]) ps
        rs `shouldBe` [ Catastrophe 3 "something broke\n"
                      , Failure "ohnoes\n"
                      , Success "yay\n"]
  where
    withSetup =
        withGitRepo
        . withCopy (fixtures </> "plugins") ("plugins" </> "git-vogue")

testGitVCS :: VCS IO -> Spec
testGitVCS VCS{..} = do
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

-- | Copy a dir and continue along
withCopy :: FilePath
         -> FilePath
         -> IO ()
         -> IO ()
withCopy src dst f = do
    void $ rawSystem "mkdir" ["plugins"]
    void $ rawSystem "cp" ["-r", src,  dst]
    f

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

-- --
-- -- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
-- --
-- -- The code in this file, and the program it is a part of, is
-- -- made available to you by its authors as open source software:
-- -- you can redistribute it and/or modify it under the terms of
-- -- the 3-clause BSD licence.
-- --
--
-- {-# LANGUAGE FlexibleContexts  #-}
-- {-# LANGUAGE OverloadedStrings #-}
--
-- -- | Description: Test git repository setup.
-- module Main where
--
-- import           Control.Monad.IO.Class ()
-- import           Data.Monoid
-- import           System.FilePath
-- import           Test.Hspec
--
-- import           Git.Vogue.Plugins
-- import           Git.Vogue.Types
--
-- failingExecutor, succeedingExecutor :: b -> IO (Status a)
-- failingExecutor    = const . return $ Failure mempty mempty
-- succeedingExecutor = const . return $ Success mempty mempty
--
-- main :: IO ()
-- main = hspec $ do
--     describe "module collation" $ do
--         it "maximum of statuses is failure" $
--             maximum [Success mempty mempty, Failure mempty mempty]
--                 `shouldBe` Failure mempty mempty
--
--         it "fails if one module fails" $ do
--             getWorst failingExecutor ["a", "b"]
--               >>= (`shouldBe` Failure mempty "\ESC[33m failed\ESC[0m with:\n\n\ESC[33m failed\ESC[0m with:\n\n")
--             getWorst failingExecutor  ["monkey"]
--               >>= (`shouldBe` Failure mempty "\ESC[33m failed\ESC[0m with:\n\n")
--
--         it "succeeds if all modules succeed" $ do
--             getWorst succeedingExecutor ["a", "b"]
--               >>= (`shouldBe` Success mempty "\ESC[32m succeeded\ESC[0m with:\n\n\ESC[32m succeeded\ESC[0m with:\n\n")
--             getWorst succeedingExecutor ["monkey"]
--               >>= (`shouldBe` Success mempty "\ESC[32m succeeded\ESC[0m with:\n\n")
--
--     describe "IO module executor" $ do
--         describe "check" $ do
--             it "fails on failing module" $
--                 runCheckExecutor FindAll "failing"
--                                 (Failure "failing" "ohnoes\n")
--
--             it "fails on succeeding module" $
--                 runCheckExecutor FindAll "succeeding"
--                                 (Success "succeeding" "yay\n")
--
--             it "fails on exploding module" $
--                 runCheckExecutor FindAll "exploding"
--                                 (Catastrophe 3 "exploding" "something broke\n")
--
--         describe "fix" $ do
--             it "fails on failing module" $
--                 runFixExecutor FindAll "failing"
--                                 (Failure "failing" "ohnoes\n")
--
--             it "fails on succeeding module" $
--                 runFixExecutor FindAll "succeeding"
--                                 (Success "succeeding" "yay\n")
--
--             it "fails on exploding module" $
--                 runFixExecutor FindAll "exploding"
--                                 (Catastrophe 3 "exploding" "something broke\n")
--
-- runCheckExecutor :: SearchMode -> FilePath -> Status Check -> Expectation
-- runCheckExecutor = runTestExecutor executeCheck
--
-- runFixExecutor :: SearchMode -> FilePath -> Status Fix -> Expectation
-- runFixExecutor = runTestExecutor executeFix
--
-- runTestExecutor
--     :: (PluginExecutorImpl IO -> SearchMode -> Plugin -> IO (Status a))
--     -> SearchMode
--     -> FilePath
--     -> Status a
--     -> Expectation
-- runTestExecutor act search_mode file expected =
--     act ioPluginExecutorImpl search_mode (Plugin ("fixtures" </> file))
--       >>= (`shouldBe` expected)
