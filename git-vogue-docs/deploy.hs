module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text hiding (map)
import Data.Time
import Shelly
import System.IO.Temp
import System.Process

main :: IO ()
main = do
    generateSite
    deploySite
    putStrLn "Deployed."

sourceRepository :: String
sourceRepository = "anchor/git-vogue"

targetRepository :: String
targetRepository = "rtrvrtg/git-vogue"

generateSite :: IO ()
generateSite = do
    _ <- system "cabal exec site clean && cabal exec site build"
    return ()

deploySite :: IO ()
deploySite = withSystemTempDirectory "git-vogue-deploy" deploy
  where
    deploy :: Prelude.FilePath -> IO ()
    deploy fp = do
        t <- getCurrentTime
        let shfp = pack fp
        _ <- shelly (shrun t shfp)
        return ()
    shrun t shfp = do
        let dest_fp = fromText shfp
        void $ cp_r (fromText . pack $ "_site") dest_fp
        chdir dest_fp $ do
            _ <- sequence . map runCmd $ [
                ("git",["init"]),
                ("git",["add","."]),
                ("git",["commit","-m","Site updated at " ++ show t]),
                ("git",["remote","add","origin","git@github.com:" ++ targetRepository ++ ".git"]),
                ("git",["push","origin","master:gh-pages","--force"])]
            return ()
        return shfp

runCmd :: (String, [String]) -> ShIO ()
runCmd (c,p) = do
    _ <- run (fromText . pack $ c) (map pack p)
    return ()
