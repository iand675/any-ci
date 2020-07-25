{-# LANGUAGE GADTs #-}
module Buildkite.Steps where
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty)

data Pipeline = Pipeline

-- Expression stuff
data Regex
data Expression a where
  (:==) :: Expression a -> Expression a -> Expression Bool
  (:!=) :: Expression a -> Expression a -> Expression Bool
  (:=~) :: Expression String -> Expression Regex -> Expression Bool
  (:!~) :: Expression String -> Expression Regex -> Expression Bool
  (:||) :: Expression Bool -> Expression Bool -> Expression Bool
  (:&&) :: Expression Bool -> Expression Bool -> Expression Bool
  Includes :: Expression [a] -> Expression a -> Expression Bool
  Int :: Integer -> Expression Integer
  Str :: String -> Expression String
  Bool :: Bool -> Expression Bool
  Null :: Expression (Maybe a)
  NotNull :: a -> Expression (Maybe a)
  Regex :: String -> Expression Regex
  Not :: Expression Bool -> Expression Bool
  Variable :: String -> Expression a

buildAuthorEmail :: Expression String
buildAuthorEmail = Variable "build.author.email"

buildAuthorId :: Expression String
buildAuthorId = Variable "build.author.id"

buildAuthorName :: Expression String
buildAuthorName = Variable "build.author.name"

buildAuthorTeams :: Expression [String]
buildAuthorTeams = Variable "build.author.teams"

buildBranch :: Expression String
buildBranch = Variable "build.branch"

-- | The commit number of the commit the current build is based on
buildCommit :: Expression String
buildCommit = Variable "build.commit"

-- | The email of the user who created the build
buildCreatorEmail :: Expression String
buildCreatorEmail = Variable "build.creator.email"

-- | The ID of the user who created the build
buildCreatorId :: Expression String
buildCreatorId = Variable "build.creator.id"

-- | The name of the user who created the build
buildCreatorName :: Expression String
buildCreatorName = Variable "build.creator.name"

-- | An array of the team/s which the user who created the build is a member of
buildCreatorTeams :: Expression [String]
buildCreatorTeams = Variable "build.creator.teams"

-- | This function returns the value of the environment passed as the first argument, otherwise null if the environment variable has not been set
-- TODO build.env()	String, null	

-- | String	The ID of the current build
buildId :: Expression String
buildId = Variable "build.id"

-- | The current build's message
buildMessage :: Expression (Maybe String)
buildMessage = Variable "build.message"

-- | The number of the current build
buildNumber :: Expression Integer
buildNumber = Variable "build.number"

-- | The base branch that the pull request is targeting, otherwise null if the branch is not a pull request
buildPullRequestBaseBranch :: Expression (Maybe String)
buildPullRequestBaseBranch = Variable "build.pull_request.base_branch"

-- | The number of the pull request, otherwise null if the branch is not a pull request
buildPullRequestId :: Expression (Maybe String)
buildPullRequestId = Variable "buildbuild.pull_request.id"

-- | If the pull request is a draft, otherwise null if the branch is not a pull request or the provider doesn't support draft pull requests
buildPullRequestDraft :: Expression (Maybe Boolean)
buildPullRequestDraft = Variable "build.pull_request.draft"

-- | The repository URL of the pull request, otherwise null if the branch is not a pull request
buildPullRequestRepository :: Expression (Maybe String)
buildPullRequestRepository = Variable "build.pull_request.repository"

-- | If the pull request comes from a forked repository, otherwise null if the branch is not a pull request
build.pull_request.repository.fork	Boolean, null	

-- | The source of the event that created the build
--
-- Available sources: ui, api, webhook, trigger_job, schedule
build.source	String

-- | The tag associated with the commit the current build is based on
build.tag	String, null

-- | The default branch of the pipeline the current build is from
pipeline.default_branch	String, null

-- | The ID of the pipeline the current build is from
pipeline.id	String
-- | The repository of the pipeline the current build is from
pipeline.repository	String, null

-- | The slug of the pipeline the current build is from
pipeline.slug	String
-- | The ID of the organization the current build is running in
organization.id	String
-- | The slug of the organization the current build is running in
organization.slug	String











data Step
  = Command
  | Wait
  | Block
  | Input
  | Trigger

data RetryCondition = AutomaticRetry | ManualRetry
data Skip = Skip | Don'tSkip | SkipWithReason String

data CommandStep = CommandStep
  { commands :: NonEmpty String
  , agents :: HashMap String String
  , allowDependencyFailure :: Maybe Bool
  , artifactPaths :: [String]
  , branches :: [String]
  , concurrency :: Maybe Int
  , concurrencyGroup :: Maybe String
  , dependsOn :: Maybe String
  , env :: Maybe (HashMap String String)
  , if_ :: Maybe (Expression Bool)
  -- Alias: identifier
  , key :: Maybe String
  , label :: Maybe String
  , parallelism :: Maybe Int
  , plugins :: Maybe Object
  , retry :: Maybe RetryCondition
  , skip :: Maybe Skip
  , softFail :: Maybe (Either Bool [Int])
  , timeoutInMinutes :: Maybe Int
  }

data WaitStep

data BlockStep

data InputStep

data TriggerStep
