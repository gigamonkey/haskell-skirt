import Options.Applicative
import System.Directory
import System.Exit
import System.FilePath

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

main = execParser opts >>= invoke

-- Basic deal is we walk up from the directory where skirt was invoked
-- until we find the pants executable. The path from where pants lives
-- to where we were invoked tells us the default target to use.

invoke :: Invocation -> IO ()
invoke (Invocation goal target pants) = do
  here <- getCurrentDirectory
  root <- findRoot (dirs here) pants
  doIt root here
      where doIt Nothing _ = do
	      putStrLn "No pants! Better hope this is a bad dream."
	      exitFailure

	    doIt (Just r) h = do
	      -- TODO: obviously at some point this will exec pants instead of just the command.
	      setCurrentDirectory r
	      putStrLn $ pantsCommand pants goal target (pathToHere r h)
	      exitSuccess

-- Filename manipulations

findRoot [] _ = return Nothing
findRoot (d : ds) f = do
  exists <- doesFileExist (combine d f)
  if exists then return (Just d) else findRoot ds f

pathToHere root = drop (length root + 1)

dirs d = if up == d then [d] else d : dirs up where up = takeDirectory d

-- Pants command computation

pantsCommand pants goal target path = "./" ++ pants ++ " goal " ++ goal ++ " " ++ fullTarget target path

fullTarget (Just t) path = path ++ ":" ++ t
fullTarget Nothing path  = path
