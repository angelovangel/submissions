#! /usr/bin/env bash

# sole purpose is to run update_submissions.R using env variables

# dump PacBio data
echo "[$(date +'%Y-%m-%d %H:%M:%S')] - running update_submissions.R"

source .Renviron
[ ! -z $usertoken ] && ./update_submissions.R ./data/df25.rds 14 $usertoken  || echo 'usertoken variable not found!'

