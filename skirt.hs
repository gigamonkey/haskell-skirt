import System.Directory

magic = ".gitconfig" -- really should be "pants"

-- Walk up the directory tree from where we are executed to find the root.

findFileUp :: String -> String -> IO (Maybe (FilePath, FilePath))
findFileUp s f = do
  exists <- doesFileExist f
  pwd    <- getCurrentDirectory
  if exists then
      return (Just (pwd, (drop ((length pwd) + 1) s)))
  else do
    setCurrentDirectory ".."
    up <- getCurrentDirectory
    if up == pwd then return Nothing else findFileUp s f

-- Basic structure:

-- skirt [<goal>] [<target>] => ./pants goal <goal> <target> <path to current location>:<target>
-- goal defaults to 'compile'
-- target defaults to no target so we get the default target for the BUILD file via pants's defaulting mechanism.

-- Certain goals can trigger rewriting of the path (e.g. the test goal uses the test path corresponding to the current path.) or adding options.

-- skirt help <goal>: emit help messages specific to the given goal, including any options available with that goal.

-- skirt => ./pants goal compile <path>
-- skirt test ./pants goal test <testpath>
-- skirt


main = do
  pwd <- getCurrentDirectory
  root <- findFileUp pwd magic
  putStrLn $ show root
