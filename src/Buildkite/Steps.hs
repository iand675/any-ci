module Buildkite.Steps where
import Data.Yaml
import Buildkite.Steps.Types
import System.FilePath
import System.Exit
import System.Process.Typed


writePipelineFile :: FilePath -> Pipeline -> IO ()
writePipelineFile fp p = encodeFile fp p

uploadPipeline :: FilePath -> IO ExitCode
uploadPipeline fp = do
  p <- startProcess $ proc exeFile processArgs
  waitExitCode p
  where
    exeFile = "buildkite-agent"
    processArgs = ["pipeline", "upload", fp]
