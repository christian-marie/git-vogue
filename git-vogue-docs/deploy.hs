module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text hiding (drop,head,map)
import Data.Time
import Shelly
import System.IO.Temp
import System.Process

main :: IO ()
main = do
    generateSite
    deploySite
    putStrLn "Deployed."

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
        let repo = "git@github.com:" ++ targetRepository ++ ".git"
        void $ cp_r (fromText . pack $ "_site") dest_fp
        chdir dest_fp $ do
            _ <- sequence . map runCmd $ [
                ["git","init"],
                ["git","add","."],
                ["git","commit","-m","Site updated at " ++ show t],
                ["git","remote","add","origin",repo],
                ["git","push","origin","master:gh-pages","--force"] ]
            return ()
        return shfp

runCmd :: [String] -> ShIO ()
runCmd a = do
    _ <- run (fromText . pack . head $ a) (map pack . drop 1 $ a)
    return ()
