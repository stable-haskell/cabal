#!/bin/bash

set -eux

upstream_repo=$1

{

git remote add upstream "${upstream_repo}" || true
git fetch upstream

output_branches=()

# rebase each branch
mkdir -p rebase
while IFS= read -r branch || [[ -n $branch ]]; do
	(
	git checkout "${branch}"
    common_ancestor=$(git merge-base "${branch}" "origin/master")
	[ -e rebase/"${branch}" ] && exit 1
    mkdir -p rebase/"${branch}"
	cd rebase/"${branch}"
	echo "${common_ancestor}" > BASE_COMMIT
    git format-patch "${common_ancestor}".."${branch}"
	if compgen -G "*.patch" > /dev/null; then
      git reset --hard "upstream/master"
      git am --3way *.patch
	fi
    )
	output_branches+=( "${branch}" )
done < <(gh pr list --label rebase --state open --json headRefName --template '{{range .}}{{tablerow .headRefName}}{{end}}')

# sync master with upstream
git checkout master
git reset --hard upstream/master
output_branches+=( "master" )

} >&2

(
  IFS=$'\n'
  echo "${output_branches[*]}"
)

