#!/bin/bash
set -v

# ERROR HANDLING SECTION
#--------------------------------------------------------------------------------------------------------------#

# This function is ran when some command errors and the build fails
f () {
    errcode=$? # save the exit code as the first thing done in the trap function
    echo "error ${errcode}"
    echo "the command executing at the time of the error was"
    echo "$BASH_COMMAND"
    echo "on line ${BASH_LINENO[0]}"
    # Sends a status check of failure to github for this build
    curl -s "https://api.github.com/repos/plow-technologies/$REPO/statuses/$GIT_COMMIT?access_token=$GITHUB_ACCESS_TOKEN" \
    -H "Content-Type: application/json" \
    -X POST \
    -d "{\"state\": \"failure\", \"description\": \"Jenkins\", \"context\": \"continuous-integration/jenkins\", \"target_url\": \"https://jenkins.plowtech.net/job/$JOB_NAME/$BUILD_NUMBER/console\"}" > /dev/null
    # Sends a post to slack saying this build failed
    slackpost "${SLACK_URL}" "#brentci" "slackbot" "Build failed for ${GIT_URL}-${GIT_BRANCH}-jenkins"
    # exit the script or return to try again, etc.
    exit $errcode  # or use some other value or do return instead   
}


# NOTIFICATION SECTION
#--------------------------------------------------------------------------------------------------------------#

# This curl command sends a status check of pending to github for this build
curl -s "https://api.github.com/repos/plow-technologies/$REPO/statuses/$GIT_COMMIT?access_token=$GITHUB_ACCESS_TOKEN" \
  -H "Content-Type: application/json" \
  -X POST \
  -d "{\"state\": \"pending\", \"description\": \"Jenkins\", \"context\": \"continuous-integration/jenkins\", \"target_url\": \"https://jenkins.plowtech.net/job/$JOB_NAME/$BUILD_NUMBER/console\"}" > /dev/null
# This sends a message to slack indicating this build started
slackpost "${SLACK_URL}" "#brentci" "slackbot" "Build started for ${GIT_URL}-${GIT_BRANCH}-jenkins"

trap f ERR # This command starts a trap for all commands below until the trap is turned off. This trap will run the f function above

# Set and display Repo_Branch
echo "${GIT_URL}"
REPO_BRANCH="${REPO}-${GIT_BRANCH#*/}"
echo "Repo_Branch: ${REPO_BRANCH}"
GIT_URL_FILE="${GIT_URL%.git}"
GIT_REPO="${GIT_URL_FILE#*/}"
echo "GIT_REPO: ${GIT_REPO}"

# Ensure local git repo is on a branch and not in detached HEAD state
git fetch origin ${GIT_BRANCH#*/}
git checkout ${GIT_BRANCH#*/}
git reset --hard origin/${GIT_BRANCH#*/}
echo "git commit: $(git rev-parse HEAD)"
echo "git branch: $(git rev-parse --abbrev-ref HEAD)"


# BUILDING CODE SECTION
#--------------------------------------------------------------------------------------------------------------#

# Get the newest plow-stack
aws s3 cp --quiet s3://plow-build-tools/plow-stack ~/.local/bin/plow-stack
chmod +x ~/.local/bin/plow-stack
plow-stack :version

# Get the newest plow-build-archive and use it to upload binary to s3
aws s3 cp --quiet s3://plow-build-tools/plow-build-archive ~/.local/bin/plow-build-archive
chmod +x ~/.local/bin/plow-build-archive


stack setup
npm install --prefix test/interface/golden
npm run build --prefix test/interface/golden
stack test
npm run test --prefix test/interface/golden

# DEPLOY SECTION
#--------------------------------------------------------------------------------------------------------------#

case "${REPO_BRANCH}"
in ocaml-export-master)

      # Nothing
      printf "\n"
      echo "case match ocaml-export-master"
      stackPath=$(stack path --dist-dir);;

   *)
      printf "\n"
      echo "No case match not deploying";; esac 


# Turn off trap so slack or github doesn't stop build success
trap - ERR



#  FINAL NOTIFICATION SECTION
#--------------------------------------------------------------------------------------------------------------#

# Send successful status check to github
curl -s "https://api.github.com/repos/plow-technologies/$REPO/statuses/$GIT_COMMIT?access_token=$GITHUB_ACCESS_TOKEN" \
  -H "Content-Type: application/json" \
  -X POST \
  -d "{\"state\": \"success\", \"description\": \"Jenkins\", \"context\": \"continuous-integration/jenkins\", \"target_url\": \"https://jenkins.plowtech.net/job/$JOB_NAME/$BUILD_NUMBER/console\"}" > /dev/null
# Send successful build message to slack
slackpost "${SLACK_URL}" "#brentci" "slackbot" "Build successful for ${GIT_URL}-${GIT_BRANCH}-jenkins"
