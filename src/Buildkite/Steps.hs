module Buildkite.Steps where
import Data.List.NonEmpty (NonEmpty)
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

command :: String -> Step
command s = command' s id

command' :: String -> (CommandStep -> CommandStep) -> Step
command' s = commands' (pure s)

commands' :: NonEmpty String -> (CommandStep -> CommandStep) -> Step
commands' s f = Command $ f $ commandStep s

wait :: Step
wait = wait' id

wait' :: (WaitStep -> WaitStep) -> Step
wait' f = Wait $ f waitStep

block :: String -> Step
block s = block' s id

block' :: String -> (BlockStep -> BlockStep) -> Step
block' s f = Block $ f $ blockStep s 

input :: String -> Step
input s = input' s id

input' :: String -> (InputStep -> InputStep) -> Step
input' s f = Input $ f $ inputStep s

trigger :: String -> Step
trigger s = trigger' s id

trigger' :: String -> (TriggerStep -> TriggerStep) -> Step
trigger' s f = Trigger $ f $ triggerStep s

