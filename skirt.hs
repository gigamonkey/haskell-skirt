import Data.List
import Data.Maybe
import Options.Applicative
import System.Directory
import System.Exit
import System.FilePath
import System.Process

data Invocation = Invocation String (Maybe String) String

-- Set up command line args

tldr = "skirt - a wrapper around pants"

description = unlines [
  "A wrapper around pants that takes care of invoking pants from the root ",
  "of the repo with the target pointing back to the current working ",
  "directory. It also does some rewriting of goals (you can say 'clean' ",
  "instead of 'clean-all') and when you invoke the 'test' goal from a ",
  "src/... directory, points the target at the tests directory ",
  "corresponding to your current directory. If you don't like the wrapper ",
  "pun, you can also think of it as a way to skirt around using pants ",
  "directly. "]

invocation = Invocation <$> goal <*> optional target <*> pants
goal       = argument str (metavar "GOAL" <> help "The pants goal" <> value "compile" <> showDefault)
target     = argument str (metavar "TARGET" <> help "The target (default: main target in current directory)")
pants      = strOption (metavar "PANTS" <> help "Name of the Pants executable" <> long "pants" <> value "pants" <> showDefault)
opts       = info (helper <*> invocation) (fullDesc <> progDesc description <> header tldr)

main = execParser opts >>= invoke

-- Basic deal is we walk up from the directory where skirt was invoked
-- until we find the pants executable. The path from where pants lives
-- to where we were invoked tells us the default target to use. We can
-- also do some translation of goals and targets. At the moment this
-- is just to translate the goal 'clean-all' to 'clean' (less typing)
-- and to rewrite the path when the goal is 'test' to be under the
-- tests hierarchy rather than src. (This latter maneuver depends on
-- knowing file structure of the repo.)

invoke :: Invocation -> IO ()
invoke (Invocation goal target pants) = do
  here <- getCurrentDirectory
  root <- findRoot (dirs here) pants
  runPants root here
      where
	runPants Nothing _ = do
	  putStrLn "No pants! People are pointing and laughing. Maybe it's a bad dream."
	  exitFailure
	runPants (Just root) here = do
	  setCurrentDirectory root
	  code <- rawSystem ("./" ++ pants) ("goal" : translate goal : computeTarget goal (pathToHere root here) target)
	  exitWith code

-- Filename manipulations

findRoot [] _ = return Nothing
findRoot (d : ds) f = do
  exists <- doesFileExist (combine d f)
  if exists then return (Just d) else findRoot ds f

pathToHere root = drop (length root + 1)

dirs d = if up == d then [d] else d : dirs up where up = takeDirectory d

-- Pants command computation

computeTarget "clean" _ _        = []
computeTarget "test" path target = [ testPath path ++ fullTarget target ]
computeTarget _ path target      = [ path ++ fullTarget target ]

fullTarget t = fromMaybe "" $ (\s -> ":" ++ s) <$> t
testPath p = if isPrefixOf "src/" p then "tests/" ++ drop (length "src/") p else p

-- We translate certain goals from their pants name to something nicer.

translate "clean" = "clean-all"
translate x = x
