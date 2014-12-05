{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- | Description: Test git repository setup.
module Main where

import           Control.Exception.Lifted
import           Control.Monad.Base
import           Control.Monad.IO.Class      ()
import           Control.Monad.Trans.Control
import           Data.Monoid
import           Data.String
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           System.Posix.Temp
import           System.Process
import           Test.Hspec


data Check
data Fix

data Status a
    = Success ModuleName Text
    | Failure ModuleName Text
    | Catastrophe ModuleName Text
  deriving (Show, Ord, Eq)

colorize :: Status a -> Text
colorize (Success     (ModuleName x) y) = "\x1b[32m" <> x <> " succeeded with " <> y <> "\x1b[0m"
colorize (Failure     (ModuleName x) y) = "\x1b[33m" <> x <> " failed with "    <> y <> "\x1b[0m"
colorize (Catastrophe (ModuleName x) y) = "\x1b[31m" <> x <> " exploded with "  <> y <> "\x1b[0m"

newtype ModulePath = ModulePath {
    unModulePath :: FilePath
} deriving (Show, Ord, Eq, IsString)

newtype ModuleName = ModuleName {
    unModuleName :: Text
} deriving (Show, Ord, Eq, IsString, Monoid)

data ModuleExecutorImpl m = ModuleExecutorImpl{
    executeFix   :: ModulePath -> m (Status Fix),
    executeCheck :: ModulePath -> m (Status Check)
}

failingModuleExecutorImpl :: ModuleExecutorImpl IO
failingModuleExecutorImpl =
        ModuleExecutorImpl (const . return $ Failure mempty mempty)
                           (const . return $ Failure mempty mempty)

succeedingModuleExecutorImpl :: ModuleExecutorImpl IO
succeedingModuleExecutorImpl =
        ModuleExecutorImpl (const . return $ Success mempty mempty)
                           (const . return $ Success mempty mempty)

checkModules
    :: Monad m
    => ModuleExecutorImpl m
    -> [ModulePath]
    -> m (Status Check)
checkModules ModuleExecutorImpl{..} ps = do
    rs <- mapM executeCheck ps
    return $ insertMax rs (T.unlines $ map colorize rs)

fixModules
    :: Monad m
    => ModuleExecutorImpl m
    -> [ModulePath]
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

main :: IO ()
main = hspec . describe "Git repository setup" $ do
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
