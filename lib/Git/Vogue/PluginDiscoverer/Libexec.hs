--
-- Copyright © 2013-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

module Git.Vogue.PluginDiscoverer.Libexec
(
) where

{--

-- | Execute a plugin in IO
ioPluginExecutorImpl :: MonadIO m => PluginExecutorImpl m
ioPluginExecutorImpl =
    PluginExecutorImpl (f "fix") (f "check")
  where
    -- | Given the command sub-type, and the path to the plugin, execute it
    -- appropriately.
    --
    -- This involves the interface described in README under "Plugin design".
    f :: MonadIO m => String -> SearchMode -> Plugin -> m (Status a)
    f arg sm (Plugin path) = liftIO $ do
        name <- getName path
        fs <- unlines <$> (lines <$> paths sm >>= filterM doesFileExist)
        (status, out, err) <- readProcessWithExitCode path [arg] fs
        let glommed = fromString $ out <> err

        return $ case status of
            ExitSuccess   -> Success name glommed
            ExitFailure 1 -> Failure name glommed
            ExitFailure n -> Catastrophe n name glommed

    paths FindChanged = git ["diff", "--cached", "--name-only"]
    paths FindAll     = git ["ls-files"]

    git args = readProcess "git" args ""

    getName path = do
        (status, name, _) <- readProcessWithExitCode path ["name"] mempty
        return . PluginName . fromString . strip $ case status of
            ExitSuccess -> if null name then path else name
            ExitFailure _ -> path


runFix :: (MonadIO m, Functor m) => SearchMode -> Vogue m ()
runFix sm = do
    -- See which plugins failed first
    rs <- ask >>= mapM (\x -> (x,) <$> executeCheck ioPluginExecutorImpl sm x)
    -- Now fix the failed ones only
    getWorst (executeFix ioPluginExecutorImpl sm) [ x | (x, Failure{}) <- rs ]
    >>= outputStatusAndExit

-- | Check for broken things.
runCheck :: MonadIO m => SearchMode -> Vogue m ()
runCheck sm =
    ask
    >>= getWorst (executeCheck ioPluginExecutorImpl sm)
    >>= outputStatusAndExit

listPlugins :: MonadIO m => Vogue m ()
listPlugins = do
    dir <- liftIO ((</> "git-vogue") <$> getLibexecDir)
    liftIO . putStrLn $ "git-vogue looks for plugins in:\n\n\t"  <> dir <> "\n"
    plugins <- ask
    liftIO .  putStr
         $  "git-vogue knows about the following plugins:\n\n"
         <> unlines (fmap (('\t':) . pluginName) plugins)

-- | Get list of disabled plugins from git configuration.
disabledPlugins
    :: (Monad m, Functor m, MonadIO m)
    => m [String]
disabledPlugins = lines <$> liftIO (readConfig `catch` none)
  where
    readConfig = readProcess "git" ["config", "--get-all", "vogue.disable"] ""
    none (SomeException _) = return []


-- | Discover all available plugins.
--
-- This function inspects the $PREFIX/libexec/git-vogue directory and the
-- directories listed in the $GIT_VOGUE_PATH environmental variable (if
-- defined) and builds a 'Plugin' for the executables found.
discoverPlugins :: IO [Plugin]
discoverPlugins = do
    -- Use the environmental variable and $libexec/git-vogue/ directories as
    -- the search path.
    path <- fromMaybe "" <$> lookupEnv "GIT_VOGUE_PATH"
    libexec <- (</> "git-vogue") <$> Paths.getLibexecDir
    let directories = splitOn ":" path <> [libexec]

    -- Find all executables in the directories in path.
    plugins <- (fmap . fmap) fromString
                  (traverse ls directories >>= filterM isExecutable . concat)

    -- Filter out disabled plugins.
    disabled_plugins <- disabledPlugins
    return . filter (not . pluginIn disabled_plugins) $ plugins
  where
    ls :: FilePath -> IO [FilePath]
    ls p = do
        exists <- doesDirectoryExist p
        if exists
            then fmap (p </>) <$> getDirectoryContents p
            else return []

    isExecutable :: FilePath -> IO Bool
    isExecutable = fmap executable . getPermissions

    pluginIn :: [String] -> Plugin -> Bool
    pluginIn disabled_plugins p =
        (takeBaseName . unPlugin $ p) `elem` disabled_plugins



--}
