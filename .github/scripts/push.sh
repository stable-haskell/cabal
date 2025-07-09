#!/bin/bash

set -eux

upstream_repo=$1
base_repo=$2
output_branches=$3

git remote add upstream "${upstream_repo}" || true
git fetch upstream master

# push each branch
while IFS= read -r branch || [[ -n $branch ]]; do
	git checkout "${branch}"
	echo "git push -f ${base_repo} ${branch}"
	# git push -f "${base_repo}" "${branch}"
done < <(echo "${output_branches}")

