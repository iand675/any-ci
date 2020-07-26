module Buildkite.Steps.Variables where
import Buildkite.Steps.Types

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
