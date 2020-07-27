{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Buildkite.Steps.Types where
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import Data.String

newtype StepKey = StepKey String
  deriving (Show, Eq, Ord, ToJSON, FromJSON, IsString)

unsafeMkStepKey :: String -> StepKey
unsafeMkStepKey = StepKey

stepKey :: StepKey -> String
stepKey (StepKey s) = s

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
  showsPrec _ Buildkite.Steps.Types.Null = showString "null"
  showsPrec d (NotNull x) = showsPrec d x
  showsPrec _ (Int i) = shows i
  -- TODO better escaping
  showsPrec _ (Str s) = showString (show s)
  showsPrec _ (Buildkite.Steps.Types.Bool b) = showString $ if b then "true" else "false"
  showsPrec _ (Variable s) = showString s

instance ToJSON (Expression a) where
  toJSON = toJSON . show

data Pipeline = Pipeline
  { env :: HashMap String String
  , steps :: [Step]
  } deriving (Show)

instance ToJSON Pipeline where
  toJSON (Pipeline{..}) = object ((if null env then [] else ["env" .= env]) ++ ["steps" .= steps])

data Step
  = Command CommandStep
  | Wait WaitStep
  | Block BlockStep
  | Input InputStep
  | Trigger TriggerStep
  deriving (Show)

instance ToJSON Step where
  toJSON (Command c) = toJSON c
  toJSON (Wait w) = toJSON w
  toJSON (Block b) = toJSON b
  toJSON (Input i) = toJSON i
  toJSON (Trigger t) = toJSON t

data ExitStatus 
  = All 
  | ExitCode Int
  deriving (Show)

instance ToJSON ExitStatus where
  toJSON All = toJSON ("*" :: String)
  toJSON (ExitCode e) = toJSON e

data AutomaticRetry = RetryConditions
  { exitStatus :: Maybe ExitStatus
  , limit :: Maybe Int
  } deriving (Show)

instance ToJSON AutomaticRetry where
  toJSON (RetryConditions{..}) = object $ catMaybes
    [ ("exit_status" .=) <$> exitStatus
    , ("limit" .=) <$> limit
    ]

data ManualRetry = ManualRetryOptions
  { allowed :: Maybe Bool
  , permitOnPassed :: Maybe Bool
  , reason :: Maybe String
  } deriving (Show)

instance ToJSON ManualRetry where
  toJSON (ManualRetryOptions{..}) = object $ catMaybes
    [ ("allowed" .=) <$> allowed
    , ("permit_on_passed" .=) <$> permitOnPassed
    , ("reason" .=) <$> reason
    ]

data RetryCondition
  = AutomaticRetry (Either Bool (NonEmpty AutomaticRetry))
  | ManualRetry (Either Bool ManualRetry)
  deriving (Show)

instance ToJSON RetryCondition where
  toJSON (ManualRetry e) = either toJSON toJSON e

data Skip = Skip (Either Bool String)
  deriving (Show)

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
  , key :: Maybe StepKey
  , label :: Maybe String
  , parallelism :: Maybe Int
  , plugins :: Maybe Object
  , retry :: Maybe RetryCondition
  , skip :: Maybe Skip
  , softFail :: Maybe (Either Bool [Int])
  , timeoutInMinutes :: Maybe Int
  } deriving (Show)

commandStep :: NonEmpty String -> CommandStep
commandStep l = CommandStep
  { commands = l
  , agents = mempty
  , allowDependencyFailure = Nothing
  , artifactPaths = mempty
  , branches = mempty
  , concurrency = Nothing
  , concurrencyGroup = Nothing
  , dependsOn = mempty
  , env = mempty
  , if_ = Nothing
  , key = Nothing
  , label = Nothing
  , parallelism = Nothing
  , plugins = Nothing
  , retry = Nothing
  , skip = Nothing
  , softFail = Nothing
  , timeoutInMinutes = Nothing
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
  , dependsOn :: [StepKey]
  , allowDependencyFailure :: Maybe Bool
  } deriving (Show)

waitStep :: WaitStep
waitStep = WaitStep Nothing Nothing [] Nothing

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
  deriving (Show)

instance ToJSON InputField where
  toJSON (InputFieldTextInput t) = toJSON t
  toJSON (InputFieldSelectInput s) = toJSON s

data TextInput = TextInput
  { text :: String
  , key :: String
  , hint :: Maybe String
  , required :: Maybe Bool
  , default_ :: Maybe String
  } deriving (Show)

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
  } deriving (Show)

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
  } deriving (Show)

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
  , dependsOn :: [StepKey]
  , allowDependencyFailure :: Maybe Bool
  } deriving (Show)

blockStep :: String -> BlockStep
blockStep l = BlockStep
  { label = l
  , prompt = Nothing
  , fields = []
  , branches = []
  , if_ = Nothing
  , dependsOn = []
  , allowDependencyFailure = Nothing
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
  , dependsOn :: [StepKey]
  , allowDependencyFailure :: Maybe Bool
  } deriving (Show)

inputStep :: String -> InputStep
inputStep l = InputStep
  { label = l
  , prompt = Nothing
  , fields = []
  , branches = []
  , if_ = Nothing
  , dependsOn = []
  , allowDependencyFailure = Nothing
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
  } deriving (Show)

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
  , dependsOn :: [StepKey]
  , allowDependencyFailure :: Maybe Bool
  } deriving (Show)

triggerStep :: String -> TriggerStep
triggerStep t = TriggerStep
  { trigger = t
  , build = BuildAttributes Nothing Nothing mempty mempty mempty
  , async = Nothing
  , branches = []
  , if_ = Nothing
  , dependsOn = []
  , allowDependencyFailure = Nothing
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