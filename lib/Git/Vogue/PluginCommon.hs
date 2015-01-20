--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE TupleSections #-}

-- | Common helpers for git vogue plugins
module Git.Vogue.PluginCommon
(
    forWithKey_,
    forWithKey,
    hsProjects,
    getPluginCommand,
    pureSubCommand,
    PluginCommand(..),
) where

import           Control.Applicative
import           Data.Functor
import           Data.List
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Monoid
import           Options.Applicative

import           Data.ListTrie.Patricia.Map.Eq (TrieMap, deleteSuffixes,
                                                fromList, lookupPrefix, toList)
import           Data.Ord
import           System.FilePath.Posix

-- | Helper for traversing a Map with keys
forWithKey_ :: Applicative f => Map k v -> (k -> v -> f ()) -> f ()
forWithKey_ m a = void $ M.traverseWithKey a m

forWithKey :: Applicative f => Map k v -> (k -> v -> f a) -> f (Map k a)
forWithKey = flip M.traverseWithKey

-- | Find .cabal files in hsFiles and arrange children underneath these
-- "headings".
hsProjects
    :: [FilePath] -- ^ Files to be checked
    -> [FilePath] -- ^ All files
    -> Map FilePath [FilePath]
hsProjects check_fs all_fs =
    -- We want to stick the subset of files to be checked under the same
    -- project headings as we would if we were checking all files. So we mush
    -- them together.
    --
    -- Discard the remainder, the user probably doesn't know what to do with
    -- it.
    let (complete_proj_map, _) = findProjects (isSuffixOf ".cabal") all_fs
    -- Now do the awesome quadratic thing and traverse lists.
        proj_map =  fmap (filter (`elem` check_fs)) complete_proj_map
    -- And finally strip the prefixes of the dirs, so that this looks a bit
    -- like a one level trie.
        bug = error "BUG: hsProjects: A key was not a prefix of its elements"
    in M.mapWithKey (\k -> fmap (fromMaybe bug . stripPrefix k)) proj_map



-- | Given a predicate to identify a file as being in the "root" of a
-- directory and a bunch of FilePaths, figure out which file paths belong under
-- these roots and "compartmentalize" them. The remainder of possibly
-- un-accounted-for files are the second element returned.
--
-- This is useful for finding files belonging to distinct projects within a
-- repository.
findProjects
    :: (FilePath -> Bool)
    -> [FilePath]
    -> (Map FilePath [FilePath], [FilePath])
findProjects p xs =
        -- We start out by putting all of the files in a trie.
        --
        -- Note that we tack on a / for everything so that they share a common
        -- root node.
    let all_trie = unFlatten (fmap (splitPath . ('/':)) xs)

        -- Now we find all of the project roots. Again tacking on the root so
        -- that init is safe and everything lines up.
        roots = sortBy (comparing length) . fmap (init . splitPath . ('/':)) $
                    filter p xs

        -- Now iterate over the project roots, taking the chunks of the tree
        -- out that belong under that as we go. It's simpler than it looks.
        f x (rs, t) =
            (M.insertWith (<>)
                          (joinPath  $ tail x)
                          ((fmap (joinPath . tail) . flatten) (lookupPrefix x t))
                          rs
            , deleteSuffixes x t)
        (projects,remainder) = foldr f (mempty, all_trie) roots

    -- Now put the broken up paths back together and take the roots off.
    in (projects
       ,fmap (joinPath . tail) . flatten $ remainder)
  where
    -- Stuff a list of keys into a trie with dummy values.
    unFlatten :: Eq k => [[k]] -> TrieMap k ()
    unFlatten = fromList . fmap (,())

    -- Extract a list of keys from a trie, throwing away the values.
    flatten :: Eq k => TrieMap k a -> [[k]]
    flatten = fmap fst . toList

-- | Parser for plugin arguments
pluginCommandParser :: Parser PluginCommand
pluginCommandParser = subparser
    (  pureSubCommand "name" CmdName "Get name of plugin"
    <> fpCommand "check" CmdCheck "Check for problems"
    <> fpCommand "fix" CmdFix "Try to fix problems"
    )

-- Helper for plugin commands that take [FilePath]s
fpCommand
    :: String
    -> ([FilePath] -> [FilePath] -> a)
    -> String
    -> Mod CommandFields a
fpCommand name ctor desc = command name (info parser (progDesc desc))
  where
    parser = ctor <$> argument (lines <$> str) (metavar "CHECKABLE_FILES")
                  <*> argument (lines <$> str) (metavar "ALL_FILES")

-- | Sub-command helper
pureSubCommand :: String -> a -> String -> Mod CommandFields a
pureSubCommand name ctor desc = command name (info (pure ctor) (progDesc desc))

-- | Get the plugin command requested given a header and a description
getPluginCommand :: String -> String -> IO PluginCommand
getPluginCommand hdr desc = execParser parser
  where
    parser = info (helper <*> pluginCommandParser)
        ( fullDesc
        <> progDesc desc
        <> header hdr)

-- | Arguments to the plugin
data PluginCommand
    -- | Check the project for problems.
    = CmdCheck [FilePath] [FilePath]
    -- | Fix problems in the project.
    | CmdFix [FilePath] [FilePath]
    -- | Report details.
    | CmdName


