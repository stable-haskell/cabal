#!/bin/bash

set -eux

upstream_repo=$1
# of the form [<branch>:<rebase-target>], e.g.:
#   BA:master B1:master B2:B1 B3:B2
spec=$2

{

git remote add upstream "${upstream_repo}" || true
git fetch upstream

output_branches=()

# sync master with upstream
git checkout master
git reset --hard upstream/master
output_branches+=( "master" )

# rebase each branch
mkdir -p rebase
for branch_spec in ${spec} ; do
	branch=$(echo "${branch_spec}" | awk -F ':' '{ print $1 }')
	rebase_target=$(echo "${branch_spec}" | awk -F ':' '{ print $2 }')
	git checkout "${branch}"
    common_ancestor=$(git merge-base "${branch}" "origin/${rebase_target}")
	[ -e rebase/"${branch}" ] && exit 1
    mkdir -p rebase/"${branch}"
	(
	cd rebase/"${branch}"
	echo "${common_ancestor}" > BASE_COMMIT
    git format-patch "${common_ancestor}".."${branch}"
    )
	if compgen -G rebase/"${branch}"/*.patch > /dev/null; then
	  git reset --hard "${rebase_target}"
      git am --3way rebase/"${branch}"/*.patch
	fi
	output_branches+=( "${branch}" )
done
unset branch_spec branch

# cherry-pick on stable-master
git checkout stable-master
git reset --hard upstream/master
for branch_spec in ${spec} ; do
    branch=$(echo "${branch_spec}" | awk -F ':' '{ print $1 }')
    if compgen -G "rebase/${branch}"/*.patch > /dev/null; then
        git am --3way "rebase/${branch}"/*.patch
    fi
done
output_branches+=( "stable-master" )

} >&2

echo "${output_branches[*]}"

