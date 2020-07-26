{-# LANGUAGE DuplicateRecordFields #-}
module Main where
import Buildkite.Steps
import Buildkite.Steps.Types

main :: IO ()
main = do
  writePipelineRelativeTo "." $ Pipeline mempty
    [ Command $ (commandStep $ pure "stack build")
      { key = Just "initial_build"
      }
    , Command $ (commandStep $ pure "stack exec -- buildkite-exe")
      { key = Just "gen_pipeline"
      , dependsOn = pure "initial_build"
      }
    , Command $ (commandStep $ pure "echo hello")
      { key = Just "gen_pipeline"
      , dependsOn = pure "gen_pipeline"
      }
    ]
    
  uploadPipeline ".buildkite/pipeline.yml" >>= print
