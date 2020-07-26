module Buildkite.Steps where
import Data.Yaml
import Buildkite.Steps.Types
import System.FilePath

writePipelineRelativeTo :: FilePath -> Pipeline -> IO ()
writePipelineRelativeTo fp p = encodeFile (fp </> ".buildkite" </> "pipeline.yml") p
