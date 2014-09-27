import Options.Applicative

data Sample = Sample String Bool
--  { hello :: String
--  , quiet :: Bool }

sample :: Parser Sample
sample = Sample
  <$> strOption
      ( long "hello"
     <> metavar "TARGET"
     <> help "Target for the greeting" )
  <*> switch
      ( long "quiet"
     <> help "Whether to be quiet" )

greet :: Sample -> IO ()
greet (Sample h False) = putStrLn $ "Hello, " ++ h
greet _ = return ()

main :: IO ()
main = execParser opts >>= greet
  where
    opts = info (helper <*> sample)
      ( fullDesc
     <> progDesc "Invokes pants"
     <> header "skirt - a wrapper around pants" )
