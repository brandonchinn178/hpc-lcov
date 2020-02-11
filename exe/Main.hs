{-# LANGUAGE RecordWildCards #-}

import Control.Monad (forM)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as ByteString
import Data.HashMap.Lazy ((!))
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Yaml as Yaml
import qualified Options.Applicative as Opt
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (<.>), (</>))
import System.Process (readProcess)
import Trace.Hpc.Codecov (generateCodecovFromTix)
import Trace.Hpc.Mix (Mix, readMix)
import Trace.Hpc.Tix (Tix(..), TixModule, readTix, tixModuleName)

data CLIOptions = CLIOptions
  { cliTargets :: [String]
  , cliOutput  :: FilePath
  }

getCLIOptions :: IO CLIOptions
getCLIOptions = Opt.execParser
  $ Opt.info (Opt.helper <*> parseCLIOptions) $ Opt.progDesc description
  where
    parseCLIOptions = CLIOptions
      <$> parseCLITargets
      <*> parseCLIOutput
    parseCLITargets = Opt.many $ Opt.strArgument $ mconcat
      [ Opt.metavar "TARGET"
      , Opt.help "The test-suite(s) to get coverage information for, as `package:test-suite`"
      ]
    parseCLIOutput = Opt.strOption $ mconcat
      [ Opt.long "output"
      , Opt.short 'o'
      , Opt.metavar "FILE"
      , Opt.help "The file to save coverage information (default: coverage.json)"
      , Opt.value "coverage.json"
      ]

    description = "Convert HPC coverage output into Codecov JSON coverage format"

main :: IO ()
main = do
  CLIOptions{..} <- getCLIOptions

  packages <- case partitionMaybes parseTarget cliTargets of
    (packages, []) -> return packages
    (_, missing) -> fail $ "Invalid target(s): " ++ intercalate ", " missing

  (moduleToMixList, tixModulesList) <- fmap unzip $ forM packages $ \(packageName, testName) -> do
    tixFilePath <- getTixFilePath packageName testName
    Tix tixModules <- readTix tixFilePath >>=
      maybe (fail $ "Could not find tix file: " ++ show tixFilePath) return

    mixDirectory <- getMixDirectory packageName
    moduleToMix <- forM tixModules $ \tixModule -> do
      mixFile <- findMixFile mixDirectory tixModule
      return (tixModuleName tixModule, mixFile)

    return (moduleToMix, tixModules)

  let tixModules = concat tixModulesList
      moduleToMix = Map.toList . Map.fromListWith checkDupeMix . concat $ moduleToMixList
      checkDupeMix mix1 mix2 = if mix1 == mix2
        then mix1
        else error $ ".mix files differ: " ++ show (mix1, mix2)
      report = generateCodecovFromTix moduleToMix tixModules

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
  stackConfig <- Yaml.decodeFileEither stackConfigPath >>=
    either (\e -> fail $ "Could not decode file `" ++ stackConfigPath ++ "`: " ++ show e) return

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

partitionMaybes :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionMaybes f = go [] []
  where
    go justs nothings [] = (reverse justs, reverse nothings)
    go justs nothings (a:as) =
      case f a of
        Just b -> go (b:justs) nothings as
        Nothing -> go justs (a:nothings) as
