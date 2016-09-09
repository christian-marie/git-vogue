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
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

-- | Description: Test git repository setup.
module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Map                           (fromList)
import           System.Directory
import           System.FilePath
import           System.IO.Temp
import           System.Process
import           Test.Hspec

import           Git.Vogue.PluginCommon
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
        describe "Plugin helpers" .
            it "finds hs projects" $ do
                hsProjects ["a.hs"] ["a.cabal", "a.hs"] `shouldBe`
                    fromList [("", ["a.hs"])]

                let nested = ["a.cabal", "a.hs", "b/b.cabal", "b/b.hs"]
                join hsProjects nested `shouldBe`
                    fromList [ ("",["a.cabal","a.hs"])
                             , ("b/",["b.cabal", "b.hs"]) ]

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
        rs <- sequence $ fmap (\Plugin{..} -> runCheck ["a"] ["a"]) ps
        rs `shouldBe` [ Catastrophe 3 "something broke\n"
                      , Failure "ohnoes\n"
                      , Success "yay\n"]

    it "provides fix methods that do the expected things" . withSetup $ do
        ps <- filter enabled <$> discoverPlugins
        rs <- sequence $ fmap (\Plugin{..} -> runFix ["a"] ["a"]) ps
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

        it "should list root dir correctly" . withGitRepo' $ \dir ->
            getTopLevel >>= (`shouldBe` dir)

-- | Copy a dir and continue along
withCopy :: FilePath
         -> FilePath
         -> IO ()
         -> IO ()
withCopy src dst f = do
    void $ rawSystem "mkdir" ["plugins"]
    void $ rawSystem "cp" ["-r", src,  dst]
    f

withGitRepo
    :: IO ()
    -> IO ()
withGitRepo = withGitRepo' . const

-- | Create a git repository and run an action with it, after changing to that
-- directory.
--
-- Restores current dir on completion
withGitRepo'
    :: (String -> IO ())
    -> IO ()
withGitRepo' f =
    withSystemTempDirectory "git-setup-test." $ \temp_dir -> do
        -- For some unknown reason, setting the current directory appears to do
        -- strange things with a bracket, so we don't bracket.
        canonical_dir <- canonicalizePath temp_dir
        before_dir <- getCurrentDirectory
        void $ git ["init", canonical_dir]
        setCurrentDirectory canonical_dir
        f canonical_dir
        setCurrentDirectory before_dir
