{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad (forM)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as ByteString
import Data.HashMap.Lazy (HashMap, (!))
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import qualified Options.Applicative as Opt
import Path (Abs, Dir, File, Path, Rel, reldir, (</>))
import qualified Path
import Path.IO (listDir, listDirRecur, resolveFile')
import System.Process (readProcessWithExitCode)
import Trace.Hpc.Lcov (generateCodecovFromTix)
import Trace.Hpc.Mix (Mix(..), readMix)
import Trace.Hpc.Tix (Tix(..), TixModule, readTix, tixModuleName)

data CLIOptions = CLIOptions
  { cliTixFiles :: [FilePath]
  , cliOutput   :: FilePath
  }

getCLIOptions :: IO CLIOptions
getCLIOptions = Opt.execParser
  $ Opt.info (Opt.helper <*> parseCLIOptions) $ Opt.progDesc description
  where
    parseCLIOptions = CLIOptions
      <$> parseCLITixFiles
      <*> parseCLIOutput
    parseCLITixFiles = Opt.many $ Opt.strOption $ mconcat
      [ Opt.long "file"
      , Opt.short 'f'
      , Opt.metavar "FILE"
      , Opt.help "Manually specify .tix file(s) to convert"
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
  stackRoot <- getStackRoot

  tixFiles <- if null cliTixFiles
    then findTixModules
    else mapM resolveFile' cliTixFiles
  tixModules <- concat <$> mapM readTixPath tixFiles

  distDir <- getStackDistPath
  packages <- getPackages
  let mixDirectories = map (getMixDirectory distDir . snd) packages

  moduleToMixList <- forM tixModules $ \tixModule -> do
    Mix fileLoc _ _ _ mixEntries <- readMixPath mixDirectories (Right tixModule)
    fileLocRelPath <- Path.parseRelFile fileLoc

    modulePath <- case getPackageName tixModule `lookup` packages of
      Just packagePath -> do
        let modulePathAbs = packagePath </> fileLocRelPath
        maybe (fail $ show modulePathAbs ++ " is not a subpath of " ++ show stackRoot) return $
          Path.stripProperPrefix stackRoot modulePathAbs
      Nothing -> return fileLocRelPath

    return (tixModuleName tixModule, (Path.toFilePath modulePath, mixEntries))

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

getMixDirectory :: Path Rel Dir -> Path Abs Dir -> Path Abs Dir
getMixDirectory distDir packageDir = packageDir </> distDir </> [reldir|hpc|]

{- HPC file readers -}

readTixPath :: Path b File -> IO [TixModule]
readTixPath path = do
  Tix tixModules <- readTix (Path.toFilePath path) >>=
    maybe (fail $ "Could not find tix file: " ++ Path.toFilePath path) return
  return tixModules

readMixPath :: [Path b Dir] -> Either String TixModule -> IO Mix
readMixPath = readMix . map Path.toFilePath

{- Haskell package helpers -}

getPackageName :: TixModule -> String
getPackageName = Text.unpack . takePackageName . Text.pack . tixModuleName
  where
    -- Tix module name is either just the module name or in the format `PACKAGE-VERSION-HASH/MODULE`
    takePackageName s = case Text.splitOn "/" s of
      [packageVersionHash, _] ->
        Text.intercalate "-" . dropEnd 2 . Text.splitOn "-" $ packageVersionHash
      _ -> s

    dropEnd n xs = take (length xs - n) xs

{- Stack helpers -}

-- | Get the root of the stack project.
getStackRoot :: IO (Path Abs Dir)
getStackRoot = do
  -- assume that the stack.yaml file is at the root of the stack projet
  configPath <- readStack ["path", "--config-location"]
  Path.parent <$> Path.parseAbsFile configPath

getStackHpcRoot :: IO (Path Abs Dir)
getStackHpcRoot = Path.parseAbsDir =<< readStack ["path", "--local-hpc-root"]

getStackDistPath :: IO (Path Rel Dir)
getStackDistPath = Path.parseRelDir =<< readStack ["path", "--dist-dir"]

-- | Get a list of package names in the stack project and their location.
getPackages :: IO [(String, Path Abs Dir)]
getPackages = do
  stackConfigPath <- readStack ["path", "--config-location"]
  stackConfig <- Yaml.decodeFileEither @(HashMap String JSON.Value) stackConfigPath >>=
    either (\e -> fail $ "Could not decode file `" ++ stackConfigPath ++ "`: " ++ show e) return

  stackConfigDir <- Path.parent <$> Path.parseAbsFile stackConfigPath

  packagePaths <- maybe (fail $ "Invalid packages field: " ++ show stackConfig) return $
    JSON.parseMaybe JSON.parseJSON (stackConfig ! "packages")

  forM packagePaths $ \packagePath -> do
    packageDir <- case packagePath of
      -- special case since Path doesn't support `..`. Not spending too much effort on more complex
      -- cases
      ".." -> return $ Path.parent stackConfigDir
      package -> (stackConfigDir </>) <$> Path.parseRelDir package

    (_, files) <- listDir packageDir

    packageName <- case filter (hasExt ".cabal") files of
      [] -> fail $ "No .cabal file found in " ++ Path.toFilePath packageDir
      [cabal] -> Path.toFilePath . Path.filename <$> Path.setFileExtension "" cabal
      _ -> fail $ "Multiple .cabal files found in " ++ Path.toFilePath packageDir

    return (packageName, packageDir)

readStack :: [String] -> IO String
readStack args = do
  (_, stdout, _) <- readProcessWithExitCode "stack" args ""
  return . head . lines $ stdout

{- Utilities -}

hasExt :: String -> Path b File -> Bool
hasExt ext = (== ext) . Path.fileExtension
