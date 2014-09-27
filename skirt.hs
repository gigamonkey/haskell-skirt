import Options.Applicative
import System.Directory

data Invocation = Invocation String (Maybe String) String

-- Set up command line args

tldr = "skirt - a wrapper around pants"

description = unlines [
  "A wrapper around pants (or a way to circumvent using pants directly)",
  "that provides a better user interface over the same slow and buggy code."]

goal       = argument str (metavar "GOAL" <> help "The pants goal" <> value "compile" <> showDefault)
target     = argument str (metavar "TARGET" <> help "The target (default: main target in current directory)")
pants      = strOption (metavar "PANTS" <> help "Name of the Pants executable" <> long "pants" <> value "pants" <> showDefault)
invocation = Invocation <$> goal <*> optional target <*> pants
opts       = info (helper <*> invocation) (fullDesc <> progDesc description <> header tldr)

-- Walk up the directory tree from where we are executed to find the root.

lookUp :: String -> String -> IO (Maybe FilePath)
lookUp s f = do
  exists <- doesFileExist f
  pwd    <- getCurrentDirectory
  if exists then
      return (Just (drop (length pwd + 1) s))
  else do
    setCurrentDirectory ".."
    up <- getCurrentDirectory
    if up == pwd then return Nothing else lookUp s f

invoke :: Invocation -> IO ()
invoke (Invocation goal target pants) = do
  putStrLn $ "Goal: " ++ show goal
  putStrLn $ "Target: " ++ show target
  putStrLn $ "Pants: " ++ pants
  pwd <- getCurrentDirectory
  root <- lookUp pwd pants
  print ("./" ++ pants ++ " goal " ++ show root)


main = execParser opts >>= invoke
