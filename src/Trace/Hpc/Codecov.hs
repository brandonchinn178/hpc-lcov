module Trace.Hpc.Codecov
  ( generateCodecovFromTix
  ) where

import Trace.Hpc.Codecov.Report (CodecovReport)

-- | Generate Codecov JSON format from HPC coverage data.
generateCodecovFromTix
  :: String -- ^ The name of the test suite
  -> CodecovReport
generateCodecovFromTix = undefined
