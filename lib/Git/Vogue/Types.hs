{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Git.Vogue.Types where

import           Data.Monoid
import           Data.String
import           Data.Text   (Text)
import qualified Data.Text   as T

data Check
data Fix

data Status a
    = Success ModuleName Text
    | Failure ModuleName Text
    | Catastrophe ModuleName Text
  deriving (Show, Ord, Eq)

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
