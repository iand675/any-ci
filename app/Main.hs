{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Buildkite.Steps
import Buildkite.Steps.Types

main :: IO ()
main = do
  let fp = ".buildkite/gen.yml"
  writePipelineFile fp $ Pipeline mempty $ 
    [ command' "echo hello" $ \c -> c
      { key = Just "dynamic_test"
      , dependsOn = pure "gen_pipeline"
      }
    ]
    
  uploadPipeline fp >>= print
