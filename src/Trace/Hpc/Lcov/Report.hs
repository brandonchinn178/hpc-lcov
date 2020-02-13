{-# LANGUAGE RecordWildCards #-}

module Trace.Hpc.Lcov.Report
  ( LcovReport(..)
  , FileReport(..)
  , FunctionReport(..)
  , BranchReport(..)
  , LineReport(..)
  , writeReport
  , showReport
  ) where

import Data.List (intercalate)

-- http://ltp.sourceforge.net/coverage/lcov/geninfo.1.php
newtype LcovReport = LcovReport [FileReport]

data FileReport = FileReport
  { fileReportLocation  :: FilePath
  , fileReportFunctions :: [FunctionReport] -- anything top level is considered a function
  , fileReportBranches  :: [BranchReport]
  , fileReportLines     :: [LineReport]
  } deriving (Show, Eq)

data FunctionReport = FunctionReport
  { functionReportLine :: Int
  , functionReportName :: String
  , functionReportHits :: Integer
  } deriving (Show, Eq)

data BranchReport = BranchReport
  { branchReportLine      :: Int
  , branchReportTrueHits  :: Integer
  , branchReportFalseHits :: Integer
  } deriving (Show, Eq)

data LineReport = LineReport
  { lineReportLine :: Int
  , lineReportHits :: Integer
  } deriving (Show, Eq)

writeReport :: FilePath -> LcovReport -> IO ()
writeReport fp = writeFile fp . showReport

showReport :: LcovReport -> String
showReport (LcovReport fileReports) = unlines $ concat $ zipWith generateFileReport branchIds fileReports
  where
    branchIds = scanl (+) 0 . map (length . fileReportBranches) $ fileReports

    generateFileReport branchId FileReport{..} = concat
      [ [line "TN" []]
      , [line "SF" [fileReportLocation]]
      , map showFunctionDefinition fileReportFunctions
      , map showFunctionHits fileReportFunctions
      , [line "FNF" [show $ length fileReportFunctions]]
      , [line "FNH" [countHits functionReportHits fileReportFunctions]]
      , concat $ zipWith generateBranchReport [branchId..] fileReportBranches
      , [line "BRF" [show $ length fileReportBranches * 2]] -- multiplying by 2 for true and false branches
      , [line "BRH" [countHits branchReportHits fileReportBranches]]
      , map showLineReport fileReportLines
      , [line "LF" [show $ length fileReportLines]]
      , [line "LH" [countHits lineReportHits fileReportLines]]
      , ["end_of_record"]
      ]

    showFunctionDefinition FunctionReport{..} = line "FN" [show functionReportLine, functionReportName]

    showFunctionHits FunctionReport{..} = line "FNDA" [show functionReportHits, functionReportName]

    generateBranchReport branchId BranchReport{..} =
      [ line "BRDA" $ map show [branchReportLine, branchId, 0, fromInteger branchReportTrueHits]
      , line "BRDA" $ map show [branchReportLine, branchId, 1, fromInteger branchReportFalseHits]
      ]

    showLineReport LineReport{..} = line "DA" [show lineReportLine, show lineReportHits]

    {- Helpers -}

    line :: String -> [String] -> String
    line label info = label ++ ":" ++ intercalate "," info

    countHits :: (a -> Integer) -> [a] -> String
    countHits f = show . length . filter ((> 0) . f)

    branchReportHits BranchReport{..} = branchReportTrueHits + branchReportFalseHits
