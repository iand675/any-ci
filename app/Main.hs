{-# LANGUAGE DuplicateRecordFields #-}
module Main where
import Buildkite.Steps
import Buildkite.Steps.Types

main :: IO ()
main = do
  let fp = ".buildkite/gen.yml"
  writePipelineFile fp $ Pipeline mempty
    [ Command $ (commandStep $ pure "echo hello")
      { key = Just "dynamic_test"
      , dependsOn = pure "gen_pipeline"
      }
    ]
    
  uploadPipeline fp >>= print
