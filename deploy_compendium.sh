# exit if not running on branch 'master'
this_branch=$(git branch --show-current)

if  !([ "$this_branch" == "master" ] || [ "$TRAVIS_BRANCH" == "master" ] && [ "$TRAVIS_PULL_REQUEST" == "false" ]); then
    echo "Not on branch 'master'; exiting early"
    exit 0
fi

# save working directory to restore lat1er
orig_dir=$(pwd)

# setup vars
GIT_USER="Weecology Deploy Bot"
GIT_EMAIL="weecologydeploy@weecology.org"
REPO="MATSSdemo"
GH_REPO="github.com/weecology/$REPO.git"
LAST_COMMIT_MESSAGE=$(git log --format=%B -n 1)

# build compendium
Rscript -e 'MATSS:::write_Rprofile()'
Rscript -e 'MATSS::create_MATSS_compendium("../MATSSdemo", DEPLOY = TRUE)'
rm .Rprofile
cd ../MATSSdemo
Rscript -e 'devtools::document(".")'
Rscript -e 'devtools::install(".", upgrade = "never")'
Rscript -e 'source("analysis/pipeline.R")'
echo 'Successfully built compendium!'

# add newest compendium to the compendium repo
mkdir ../scratch
cd ../scratch
git clone git://${GH_REPO}
cp -r ../MATSSdemo/* ${REPO}
cd ${REPO}
git remote
git config user.email ${GIT_EMAIL}
git config user.name ${GIT_USER}
git add .
git commit -m "Update Compendium: Travis Build $TRAVIS_BUILD_NUMBER" -m "$LAST_COMMIT_MESSAGE"
git push "https://${COMPENDIUM_DEPLOY_TOKEN}@${GH_REPO}" master > /dev/null 2>&1

# restore previous working directory
cd ${orig_dir}
