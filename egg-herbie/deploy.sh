#!/bin/sh
set -ex

TAG=generic
if [ "_$1" = "_ubuntu-latest" ]; then
    TAG=linux
elif [ "_$1" = "_windows-latest" ]; then
    TAG=windows
elif [ "_$1" = "_macos-latest" ]; then
    TAG=osx
else
    echo "Invalid OS tag! Cannot deploy!"
    exit 4
fi

setup_git() {
    git config --global user.email "travis@travis-ci.org"
    git config --global user.name "Travis CI"
}

commit_website_files() {
    git checkout -b egg-herbie-deploy-$TAG
    git add -u
    git add target/release/* -f
    git add .travis.yml
    git commit --message "Travis build: $(git rev-parse HEAD)"
}

upload_files() {
    git remote add origin-pages https://${GITHUB_TOKEN}@github.com/oflatt/egg-herbie > /dev/null 2>&1
    git push -f --set-upstream origin-pages egg-herbie-deploy-$TAG
}

setup_git
commit_website_files
upload_files
