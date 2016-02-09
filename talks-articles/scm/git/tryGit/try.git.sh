#!/bin/bash

#####
#
echo '==== ===; \\ //     //==  ==== ===='
echo ' ||  ||/   \|/  == || ___  ||   ||'
echo ' ||  ||\   |||      \\_// ====  ||'
#
#####

# 01
## prep
mkdir trygit ; cd trygit
## axn
git init
git status

# 02
## prep
echo 'octocat.txt' > octocat.txt
## axn
git status
git add octocat.txt
git status
git commit -m 'first commit'

# 03
## prep
echo 'blue_octocat.txt' > blue_octocat.txt
echo 'red_octocat.txt' > red_octocat.txt
mkdir octofamily
echo 'baby_octocat.txt' > octofamily/baby_octocat.txt
echo 'momma_octocat.txt' > octofamily/momma_octocat.txt
## axn
git add '*.txt'
git status
git commit -m 'more octocats added'
git log | cat # '| cat' to avoid pausing in pager

# 04
## prep
##### create/sign-in your GitHub account (github.com)
###### now say your github-id is available as $GITHUB_ID
###### create a repo in it by name 'try_git'
## axn
git remote add origin git@github.com:abionic/try_git.git
git push -u origin master
git pull origin master

# 05
## prep
echo 'Some more changes psuhed in.' >> octocat.txt
echo 'octodog.txt' > octofamily/octodog.txt
## axn
git diff HEAD | cat # '| cat' to avoid pausing in pager
git add octofamily/octodog.txt
git diff --staged | cat # '| cat' to avoid pausing in pager
git reset octofamily/octodog.txt
git checkout -- octocat.txt
git branch clean_up
git checkout clean_up
git rm '*.txt'
git commit -m 'removing all octocats at any path in repo'
git checkout master
git merge clean_up
git branch -d clean_up
git push

## now you can delete the try_git repo created at github
rm -rf trygit
