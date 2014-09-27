import Options.Applicative
import System.Directory
import System.Exit

data Invocation = Invocation String (Maybe String) String

-- Set up command line args

tldr = "skirt - a wrapper around pants"

description = unlines [
  "A wrapper around pants (or a way to circumvent using pants directly)",
  "that provides a better user interface over the same slow and buggy code."]

invocation = Invocation <$> goal <*> optional target <*> pants
goal       = argument str (metavar "GOAL" <> help "The pants goal" <> value "compile" <> showDefault)
target     = argument str (metavar "TARGET" <> help "The target (default: main target in current directory)")
pants      = strOption (metavar "PANTS" <> help "Name of the Pants executable" <> long "pants" <> value "pants" <> showDefault)
opts       = info (helper <*> invocation) (fullDesc <> progDesc description <> header tldr)

-- File system walking: we walk up the directory structure from where
-- skirt was invoked to find the root where the pants executable lives.

findRoot s f = do
  exists <- doesFileExist f
  pwd    <- getCurrentDirectory
  if exists then
      return (Just pwd)
  else do
    setCurrentDirectory ".."
    up <- getCurrentDirectory
    if up == pwd then return Nothing else findRoot s f

-- The default target given that we started in `start' and found pants in `root'.

invoke :: Invocation -> IO ()
invoke (Invocation goal target pants) = do
  here <- getCurrentDirectory
  root <- findRoot here pants -- N.B. this also cd's us up to the root
  doIt root here
      where doIt Nothing _ = do
	      putStrLn "No pants! Better hope this is a bad dream."
	      exitFailure

	    doIt (Just r) h = do
	      -- TODO: obviously at some point this will exec pants instead of just the command.
	      putStrLn $ pantsInvocation pants goal target (pathToHere r h)
	      exitSuccess

pantsInvocation pants goal target path = "./" ++ pants ++ " goal " ++ goal ++ " " ++ fullTarget target path

fullTarget (Just t) path = path ++ ":" ++ t
fullTarget Nothing path  = path

pathToHere root = drop (length root + 1)

main = execParser opts >>= invoke
