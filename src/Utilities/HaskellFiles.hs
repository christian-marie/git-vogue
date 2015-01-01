module Utilities.HaskellFiles where

import           Control.Monad
import           System.Directory
import           System.FilePath

-- | Get all source files recursively within a single directory.
-- To use across multiple directories, you can run the following:
--
-- `mapM getSourceFilesForDir ["./foo", "./bar"] >>= concat`
getSourceFilesForDir :: FilePath -> IO [FilePath]
getSourceFilesForDir dir = do
    contents <- getDirectoryContents dir
    let properNames = filter (`notElem` [".", ".."]) contents
    paths <- forM properNames $ \name -> do
        let path = dir </> name
        isDirectory <- doesDirectoryExist path
        return $ if isDirectory
            then ([path],[])
            else ([],[path])
    childFiles <- mapM getSourceFilesForDir (concatMap fst paths)
    let localFiles = concatMap snd paths
    return $
        concat childFiles ++
        filter ((==) ".hs" . takeExtension) localFiles