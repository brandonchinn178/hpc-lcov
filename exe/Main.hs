{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (forM)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as ByteString
import Data.HashMap.Lazy ((!))
import qualified Data.Map as Map
import qualified Data.Yaml as Yaml
import qualified Options.Applicative as Opt
import Path (Abs, Dir, File, Path, Rel, reldir, (</>))
import qualified Path
import Path.IO (listDir, listDirRecur)
import System.Process (readProcess)
import Trace.Hpc.Codecov (generateCodecovFromTix)
import Trace.Hpc.Mix (Mix(..), readMix)
import Trace.Hpc.Tix (Tix(..), TixModule, readTix, tixModuleName)

data CLIOptions = CLIOptions
  { cliOutput :: FilePath
  }

getCLIOptions :: IO CLIOptions
getCLIOptions = Opt.execParser
  $ Opt.info (Opt.helper <*> parseCLIOptions) $ Opt.progDesc description
  where
    parseCLIOptions = CLIOptions
      <$> parseCLIOutput
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

  tixModules <- fmap concat . mapM readTixPath =<< findTixModules
  mixDirectories <- getMixDirectories

  moduleToMixList <- forM tixModules $ \tixModule -> do
    Mix fileLoc _ _ _ mixEntries <- readMixPath mixDirectories (Right tixModule)
    return (tixModuleName tixModule, (fileLoc, mixEntries))

  let moduleToMix = Map.toList . Map.fromListWith checkDupeMix $ moduleToMixList
      checkDupeMix mix1 mix2 = if mix1 == mix2
        then mix1
        else error $ ".mix files differ: " ++ show (mix1, mix2)
      report = generateCodecovFromTix moduleToMix tixModules

  ByteString.writeFile cliOutput $ JSON.encode report

{- HPC file discovery -}

-- | Find all .tix files in the HPC root.
findTixModules :: IO [Path Abs File]
findTixModules = do
  hpcRoot <- getStackHpcRoot
  (_, files) <- listDirRecur hpcRoot
  return $ filter (hasExt ".tix") files

getMixDirectories :: IO [Path Abs Dir]
getMixDirectories = do
  distDir <- getStackDistPath
  map (mkMixDir distDir) <$> getPackageDirectories
  where
    mkMixDir distDir packageDir = packageDir </> distDir </> [reldir|hpc|]

{- HPC file readers -}

readTixPath :: Path b File -> IO [TixModule]
readTixPath path = do
  Tix tixModules <- readTix (Path.toFilePath path) >>=
    maybe (fail $ "Could not find tix file: " ++ Path.toFilePath path) return
  return tixModules

readMixPath :: [Path b Dir] -> Either String TixModule -> IO Mix
readMixPath = readMix . map Path.toFilePath

{- Stack helpers -}

getStackHpcRoot :: IO (Path Abs Dir)
getStackHpcRoot = Path.parseAbsDir =<< readStack ["path", "--local-hpc-root"]

getStackDistPath :: IO (Path Rel Dir)
getStackDistPath = Path.parseRelDir =<< readStack ["path", "--dist-dir"]

getPackageDirectories :: IO [Path Abs Dir]
getPackageDirectories = do
  stackConfigPath <- readStack ["path", "--config-location"]
  stackConfig <- Yaml.decodeFileEither stackConfigPath >>=
    either (\e -> fail $ "Could not decode file `" ++ stackConfigPath ++ "`: " ++ show e) return

  stackConfigDir <- Path.parent <$> Path.parseAbsFile stackConfigPath

  case JSON.parseMaybe JSON.parseJSON $ stackConfig ! "packages" of
    Just packages -> forM packages $ \case
      -- special case since Path doesn't support `..`. Not spending too much effort on more complex
      -- cases
      ".." -> return $ Path.parent stackConfigDir
      package -> (stackConfigDir </>) <$> Path.parseRelDir package
    Nothing -> fail $ "Invalid packages field: " ++ show stackConfig

readStack :: [String] -> IO String
readStack args = head . lines <$> readProcess "stack" args ""

{- Utilities -}

hasExt :: String -> Path b File -> Bool
hasExt ext = (== ext) . Path.fileExtension
