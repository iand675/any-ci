{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Buildkite.Steps where
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))

data Pipeline = Pipeline

-- parens
-- not
-- and
-- or
-- ==
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
  NotNull :: Expression a -> Expression (Maybe a)
  Regex :: String -> Expression Regex
  Not :: Expression Bool -> Expression Bool
  Variable :: String -> Expression a

instance Show (Expression a) where
  showsPrec d (l :== r) = showParen (d >= 8) (
    showsPrec 8 l . 
    showString " == " .
    showsPrec 8 r
    )

  showsPrec d (l :!= r) = showParen (d >= 8) (
    showsPrec 8 l . 
    showString " != " .
    showsPrec 8 r
    )

  showsPrec d (l :=~ r) = showParen (d >= 9) (
    showsPrec 9 l . 
    showString " =~ " .
    showsPrec 9 r
    )

  showsPrec d (l :!~ r) = showParen (d >= 9) (
    showsPrec 9 l . 
    showString " !~ " .
    showsPrec 9 r
    )

  showsPrec d (l :&& r) = showParen (d >= 4) (
    showsPrec 4 l . 
    showString " && " .
    showsPrec 4 r
    )

  showsPrec d (l :|| r) = showParen (d >= 3) (
    showsPrec 3 l . 
    showString " || " .
    showsPrec 3 r
    )

  showsPrec d (Includes arr e) = showParen (d >= 9) (
    showsPrec 9 arr . 
    showString " includes " .
    showsPrec 9 e
    )

  showsPrec d (Not e) = showParen (d >= 15) (
    showString "!" . 
    showsPrec 15 e
    )

  showsPrec _ (Regex str) = showString str
  showsPrec _ Buildkite.Steps.Null = showString "null"
  showsPrec d (NotNull x) = showsPrec d x
  showsPrec _ (Int i) = shows i
  -- TODO better escaping
  showsPrec _ (Str s) = showString (show s)
  showsPrec _ (Buildkite.Steps.Bool b) = showString $ if b then "true" else "false"
  showsPrec _ (Variable s) = showString s

instance ToJSON (Expression a) where
  toJSON = toJSON . show

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
buildPullRequestDraft :: Expression (Maybe Bool)
buildPullRequestDraft = Variable "build.pull_request.draft"

-- | The repository URL of the pull request, otherwise null if the branch is not a pull request
buildPullRequestRepository :: Expression (Maybe String)
buildPullRequestRepository = Variable "build.pull_request.repository"

-- | If the pull request comes from a forked repository, otherwise null if the branch is not a pull request
buildPulLRequest :: Expression (Maybe Bool)
buildPulLRequest = Variable "build.pull_request.repository.fork"

-- | The source of the event that created the build
--
-- Available sources: ui, api, webhook, trigger_job, schedule
buildSource :: Expression String
buildSource = Variable "build.source"

-- | The tag associated with the commit the current build is based on
buildTag :: Expression (Maybe String)
buildTag = Variable "build.tag"

-- | The default branch of the pipeline the current build is from
pipelineDefaultBranch :: Expression (Maybe String)
pipelineDefaultBranch = Variable "pipeline.default_branch"

-- | The ID of the pipeline the current build is from
pipelineId :: Expression String
pipelineId = Variable "pipeline.id"

-- | The repository of the pipeline the current build is from
pipelineRepository :: Expression (Maybe String)
pipelineRepository = Variable "pipeline.repository"

-- | The slug of the pipeline the current build is from
pipelineSlug :: Expression String
pipelineSlug = Variable "pipeline.slug"

-- | The ID of the organization the current build is running in
organizationId :: Expression String
organizationId = Variable "organization.id"
-- | The slug of the organization the current build is running in
organizationSlug :: Expression String
organizationSlug = Variable "organization.slug"


data BuildkiteConfig = BuildkiteConfig
  { steps :: [Step]
  }

instance ToJSON BuildkiteConfig where
  toJSON (BuildkiteConfig{..}) = object ["steps" .= steps]

data Step
  = Command CommandStep
  | Wait WaitStep
  | Block BlockStep
  | Input InputStep
  | Trigger TriggerStep

instance ToJSON Step where
  toJSON (Command c) = toJSON c
  toJSON (Wait w) = toJSON w
  toJSON (Block b) = toJSON b
  toJSON (Input i) = toJSON i
  toJSON (Trigger t) = toJSON t

data ExitStatus 
  = All 
  | ExitCode Int

instance ToJSON ExitStatus where
  toJSON All = toJSON ("*" :: String)
  toJSON (ExitCode e) = toJSON e

data AutomaticRetry = RetryConditions
  { exitStatus :: Maybe ExitStatus
  , limit :: Maybe Int
  }

instance ToJSON AutomaticRetry where
  toJSON (RetryConditions{..}) = object $ catMaybes
    [ ("exit_status" .=) <$> exitStatus
    , ("limit" .=) <$> limit
    ]

data ManualRetry = ManualRetryOptions
  { allowed :: Maybe Bool
  , permitOnPassed :: Maybe Bool
  , reason :: Maybe String
  }

instance ToJSON ManualRetry where
  toJSON (ManualRetryOptions{..}) = object $ catMaybes
    [ ("allowed" .=) <$> allowed
    , ("permit_on_passed" .=) <$> permitOnPassed
    , ("reason" .=) <$> reason
    ]

data RetryCondition
  = AutomaticRetry (Either Bool (NonEmpty AutomaticRetry))
  | ManualRetry (Either Bool ManualRetry)

instance ToJSON RetryCondition where
  toJSON (ManualRetry e) = either toJSON toJSON e

data Skip = Skip (Either Bool String)
instance ToJSON Skip where
  toJSON (Skip e) = either toJSON toJSON e

data CommandStep = CommandStep
  { commands :: NonEmpty String
  , agents :: HashMap String String
  , allowDependencyFailure :: Maybe Bool
  , artifactPaths :: [String]
  , branches :: [String]
  , concurrency :: Maybe Int
  , concurrencyGroup :: Maybe String
  , dependsOn :: [String]
  , env :: HashMap String String
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

instance ToJSON CommandStep where
  toJSON CommandStep{..} = object (requiredFields ++ optionalFields)
    where
      requiredFields = 
        [ case commands of
            x :| [] -> "command" .= x
            x :| xs -> "commands" .= (x : xs)
        ]
      optionalFields = catMaybes
        [ if null agents
          then Nothing
          else Just ("agents" .= agents)
        , ("allow_dependency_failure" .=) <$> allowDependencyFailure
        , if null artifactPaths
          then Nothing
          else Just ("artifact_paths" .= artifactPaths)
        , if null branches
          then Nothing
          else Just ("branches" .= unwords branches)
        , ("concurrency" .=) <$> concurrency
        , ("concurrency_group" .=) <$> concurrencyGroup
        , if null dependsOn
          then Nothing
          else Just ("depends_on" .= dependsOn)
        , if null env
          then Nothing
          else Just ("env" .= env)
        , ("if" .=) <$> if_
        , ("key" .=) <$> key
        , ("label" .=) <$> label
        , ("parallelism" .=) <$> parallelism
        , ("plugins" .=) <$> plugins
        , ("retry" .=) <$> retry
        , ("skip" .=) <$> skip
        , (\v -> "soft_fail" .= either toJSON toJSON v) <$> softFail
        , ("timeout_in_minutes" .=) <$> timeoutInMinutes
        ]

data WaitStep = WaitStep
  { continueOnFailure :: Maybe Bool
  , if_ :: Maybe (Expression Bool)
  , dependsOn :: [String]
  , allowDependencyFailure :: Maybe Bool
  }

instance ToJSON WaitStep where
  toJSON (WaitStep Nothing Nothing [] Nothing) = String "wait"
  toJSON (WaitStep{..}) = object 
    [ "wait" .= object fields
    ]
    where 
      fields = catMaybes
        [ ("continue_on_failure" .=) <$> continueOnFailure
        , ("if" .=) <$> if_
        , if null dependsOn then Nothing else Just ("depends_on" .= dependsOn)
        , ("allow_dependency_failure" .=) <$> allowDependencyFailure
        ]
  
data InputField
  = InputFieldTextInput TextInput
  | InputFieldSelectInput SelectInput

instance ToJSON InputField where
  toJSON (InputFieldTextInput t) = toJSON t
  toJSON (InputFieldSelectInput s) = toJSON s

data TextInput = TextInput
  { text :: String
  , key :: String
  , hint :: Maybe String
  , required :: Maybe Bool
  , default_ :: Maybe String
  }

instance ToJSON TextInput where
  toJSON TextInput{..} = object (requiredFields ++ optionalFields)
    where
      requiredFields = [ "text" .= text, "key" .= key ]
      optionalFields = catMaybes
        [ ("hint" .=) <$> hint
        , ("required" .=) <$> required
        , ("default" .=) <$> default_
        ]

data SelectOption = SelectOption 
  { label :: String
  , value :: String
  }

instance ToJSON SelectOption where
  toJSON SelectOption{..} = object [ "label" .= label, "value" .= value ]

data SelectInput = SelectInput
  { select :: String
  , key :: String
  , options :: [SelectOption]
  , hint :: Maybe String
  , required :: Maybe Bool
  , default_ :: Maybe String
  , multiple :: Maybe Bool
  }

instance ToJSON SelectInput where
  toJSON SelectInput{..} = object (requiredFields ++ catMaybes optionalFields)
    where
      requiredFields = 
        [ "select" .= select 
        , "key" .= key
        , "options" .= options
        ]
      optionalFields = 
        [ ("hint" .=) <$> hint
        , ("required" .=) <$> required
        , ("default" .=) <$> default_
        , ("multiple" .=) <$> multiple
        ]

data BlockStep = BlockStep
  { label :: String
  , prompt :: Maybe String
  , fields :: [InputField]
  , branches :: [String]
  , if_ :: Maybe (Expression Bool)
  , dependsOn :: [String]
  , allowDependencyFailure :: Maybe Bool
  }

instance ToJSON BlockStep where
  toJSON BlockStep{..} = object (requiredFields ++ optionalFields)
    where
      requiredFields =
        [ "block" .= label
        ]
      optionalFields = catMaybes
        [ ("prompt" .=) <$> prompt
        , if null fields
          then Nothing
          else Just ("fields" .= fields)
        , if null branches
          then Nothing
          else Just ("branches" .= unwords branches)
        , ("if" .=) <$> if_
        , if null dependsOn
          then Nothing
          else Just ("depends_on" .= dependsOn)
        , ("allow_dependency_failure" .=) <$> allowDependencyFailure
        ]

data InputStep = InputStep
  { label :: String
  , prompt :: Maybe String
  , fields :: [InputField]
  , branches :: [String]
  , if_ :: Maybe (Expression Bool)
  , dependsOn :: [String]
  , allowDependencyFailure :: Maybe Bool
  }

instance ToJSON InputStep where
  toJSON InputStep{..} = object (requiredFields ++ optionalFields)
    where
      requiredFields =
        [ "input" .= label
        ]
      optionalFields = catMaybes
        [ ("prompt" .=) <$> prompt
        , if null fields
          then Nothing
          else Just ("fields" .= fields)
        , if null branches
          then Nothing
          else Just ("branches" .= unwords branches)
        , ("if" .=) <$> if_
        , if null dependsOn
          then Nothing
          else Just ("depends_on" .= dependsOn)
        , ("allow_dependency_failure" .=) <$> allowDependencyFailure
        ]

data BuildAttributes = BuildAttributes 
  { message :: Maybe String
  , commit :: Maybe String
  , branch :: Maybe String
  , metadata :: HashMap String String
  , env :: HashMap String String
  }

emptyBuildAttrs :: BuildAttributes -> Bool
emptyBuildAttrs BuildAttributes{..} = 
  null message && 
  null commit &&
  null branch &&
  null metadata &&
  null env

instance ToJSON BuildAttributes where
  toJSON BuildAttributes{..} = object $ catMaybes
    [ ("message" .=) <$> message
    , ("commit" .=) <$> commit
    , ("branch" .=) <$> branch
    , if null metadata
      then Nothing 
      else Just ("metadata" .= metadata)
    , if null metadata
      then Nothing 
      else Just ("env" .= env)
    ]
data TriggerStep = TriggerStep
  { trigger :: String
  , build :: BuildAttributes
  , async :: Maybe Bool
  , branches :: [String]
  , if_ :: Maybe (Expression Bool)
  , dependsOn :: [String]
  , allowDependencyFailure :: Maybe Bool
  }

instance ToJSON TriggerStep where
  toJSON TriggerStep{..} = object (requiredFields ++ optionalFields)
    where
      requiredFields =
        [ "trigger" .= trigger
        ]
      optionalFields = catMaybes
        [ if emptyBuildAttrs build
          then Nothing
          else Just ("build_attributes" .= build)
        , ("async" .=) <$> async
        , if null branches
          then Nothing
          else Just ("branches" .= branches)
        , ("if" .=) <$> if_
        , if null dependsOn
          then Nothing
          else Just ("depends_on" .= dependsOn)
        , ("allow_dependency_failure" .=) <$> allowDependencyFailure
        ]
