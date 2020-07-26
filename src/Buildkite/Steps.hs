module Buildkite.Steps where
import Data.Yaml
import Buildkite.Steps.Types
import System.FilePath
import System.Exit
import System.Process.Typed


writePipelineRelativeTo :: FilePath -> Pipeline -> IO ()
writePipelineRelativeTo fp p = encodeFile (fp </> ".buildkite" </> "pipeline.yml") p

uploadPipeline :: FilePath -> IO ExitCode
uploadPipeline fp = do
  p <- startProcess $ proc exeFile processArgs
  waitExitCode p
  where
    exeFile = "buildkite-agent"
    processArgs = ["pipeline", "upload", fp]
