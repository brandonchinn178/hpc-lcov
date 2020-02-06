{-# LANGUAGE RecordWildCards #-}

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as ByteString
import Data.HashMap.Lazy ((!))
import qualified Data.Yaml as Yaml
import qualified Options.Applicative as Opt
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (<.>), (</>))
import System.Process (readProcess)
import Trace.Hpc.Codecov (generateCodecovFromTix)
import Trace.Hpc.Mix (Mix, readMix)
import Trace.Hpc.Tix (Tix(..), TixModule, readTix)

data CLIOptions = CLIOptions
  { cliTarget :: String
  , cliOutput :: FilePath
  }

getCLIOptions :: IO CLIOptions
getCLIOptions = Opt.execParser
  $ Opt.info (Opt.helper <*> parseCLIOptions) $ Opt.progDesc description
  where
    parseCLIOptions = CLIOptions
      <$> parseCLITarget
      <*> parseCLIOutput
    parseCLITarget = Opt.strArgument $ mconcat
      [ Opt.metavar "TARGET"
      , Opt.help "The test-suite to get coverage information for, as `package:test-suite`"
      ]
    parseCLIOutput = Opt.strOption $ mconcat
      [ Opt.long "output"
      , Opt.short 'o'
      , Opt.metavar "FILE"
      , Opt.help "The file to save coverage information"
      ]

    description = "Convert HPC coverage output into Codecov JSON coverage format"

main :: IO ()
main = do
  CLIOptions{..} <- getCLIOptions

  (packageName, testName) <- maybe (fail $ "Invalid target: " ++ cliTarget) return
    $ parseTarget cliTarget

  tixFilePath <- getTixFilePath packageName testName
  Tix tixModules <- readTix tixFilePath >>=
    maybe (fail $ "Could not find tix file: " ++ show tixFilePath) return

  mixDirectory <- getMixDirectory packageName
  mixFiles <- mapM (findMixFile mixDirectory) tixModules

  let report = generateCodecovFromTix $ zip tixModules mixFiles
  ByteString.writeFile cliOutput $ JSON.encode report

parseTarget :: String -> Maybe (String, String)
parseTarget target = case break (== ':') target of
  (package, ':':test) -> Just (package, test)
  _ -> Nothing

getTixFilePath :: String -> String -> IO FilePath
getTixFilePath package test = do
  hpcRoot <- getStackHpcRoot
  return $ hpcRoot </> package </> test </> test <.> ".tix"

getMixDirectory :: String -> IO FilePath
getMixDirectory package = do
  packageDir <- getPackageDirectory package
  distDir <- getStackDistPath
  return $ packageDir </> distDir </> "hpc"

findMixFile :: FilePath -> TixModule -> IO Mix
findMixFile mixDirectory tixModule = readMix [mixDirectory] (Right tixModule)

{- Stack helpers -}

getStackHpcRoot :: IO FilePath
getStackHpcRoot = readStack ["path", "--local-hpc-root"]

getStackDistPath :: IO FilePath
getStackDistPath = readStack ["path", "--dist-dir"]

getPackageDirectory :: String -> IO FilePath
getPackageDirectory package = do
  stackConfigPath <- readStack ["path", "--config-location"]
  stackConfig <- Yaml.decodeFileThrow stackConfigPath

  let isPackage dir = doesFileExist $ dir </> package <.> "cabal"

  stackPackages <- case JSON.parseMaybe JSON.parseJSON $ stackConfig ! "packages" of
    Just packages -> return $ map (takeDirectory stackConfigPath </>) packages
    Nothing -> fail $ "Invalid packages field: " ++ show stackConfig

  findM isPackage stackPackages >>=
    maybe (fail $ "Could not find package: " ++ package) return

readStack :: [String] -> IO String
readStack args = head . lines <$> readProcess "stack" args ""

{- Utilities -}

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM f = go
  where
    go [] = return Nothing
    go (x:xs) = do
      cond <- f x
      if cond
        then return $ Just x
        else go xs
