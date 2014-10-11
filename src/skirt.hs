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

invoke (Invocation goal target pants) = do
  here <- getCurrentDirectory
  root <- findRoot here pants
  runPants root here
    where
      runPants Nothing _ = do
        putStrLn "No pants! People are pointing and laughing. Maybe it's a bad dream."
        exitFailure

      runPants (Just root) here = do
        setCurrentDirectory root
        putStrLn $ unwords (command : args)
        rawSystem command args >>= exitWith
          where
            command    = "./" ++ pants
            path       = makeRelative root here
            args       = "goal" : translate goal : targetFor goal path target ++ extraArgs goal

-- Filename manipulations

findRoot here f = firstM (\d -> doesFileExist $ combine d f) (dirs here)

firstM _ [] = return Nothing
firstM p (d:ds) = p d >>= \b -> if b then return (Just d) else firstM p ds

dirs d = if up == d then [d] else d : dirs up where up = takeDirectory d

-- We translate certain goals from their pants name to something nicer.

translate "clean" = "clean-all"
translate goal    = goal

-- Pants command computation

-- some hardwired jank for the moment.
java7compile = ["--compile-javac-args=-source", "--compile-javac-args=7", "--compile-javac-args=-target", "--compile-javac-args=7"]

targetFor "clean" _ _      = []
targetFor goal path target = [fullTarget (translatePath goal path) target]

translatePath "test" = testPath
translatePath _      = id

extraArgs "clean" = []
extraArgs "test"  = extraArgs "compile" ++ ["--no-test-junit-suppress-output"]
extraArgs _       = java7compile

fullTarget p t = fromMaybe p $ ((p ++ ":") ++) <$> t

-- Could be better: assumes a particular repo layout.

testPath p = if "src/" `isPrefixOf` p then "tests/" ++ drop (length "src/") p else p
