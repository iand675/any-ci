{-# LANGUAGE DuplicateRecordFields #-}
module Main where
import Buildkite.Steps
import Buildkite.Steps.Types

main :: IO ()
main = do
  writePipelineRelativeTo "." $ Pipeline mempty
    [ Command $ (commandStep $ pure "stack build && stack exec -- buildkite-exe")
      { key = Just "gen_pipeline"
      }
    , Command $ (commandStep $ pure "echo hello")
      { key = Just "dynamic_test"
      , dependsOn = pure "gen_pipeline"
      }
    ]
    
  uploadPipeline ".buildkite/pipeline.yml" >>= print
