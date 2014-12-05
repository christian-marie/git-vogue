{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import System.IO
import System.Process

main :: IO ()
main = do
    readme <- readProcess "git" ["show", "master~1:README.md"] ""
    writeFile "git-vogue-docs/index.markdown" (intercalate "\n" ["---", "title: git-vogue", "---", "", readme])
    putStrLn "Done."
