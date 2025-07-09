#!/bin/bash

# Branch regex we consider for rebase targets.
# For our purposes this is usually 'stable-haskell/feature/*'.
# 'master' is always considered.
branch_regex=$1
shift 1
declare -a input_branches
input_branches=( "$@" )
set -eux

[ ${#input_branches[@]} -eq 0 ] &&
	input_branches=( $(gh pr list --label rebase --state open --json headRefName --jq ".[] | select( .headRefName | match(\"${branch_regex}\")) | .headRefName" --template '{{range .}}{{tablerow .headRefName}}{{end}}') )

branch_list=( )
declare -A branch_map

# @FUNCTION: die
# @USAGE: [msg]
# @DESCRIPTION:
# Exits the shell script with status code 2
# and prints the given message in red to STDERR, if any.
die() {
    (>&2 red_message "$1")
    exit 2
}

# @FUNCTION: red_message
# @USAGE: <msg>
# @DESCRIPTION:
# Print a red message.
red_message() {
    printf "\\033[0;31m%s\\033[0m\\n" "$1"
}

# @FUNCTION: array_contains
# @USAGE: <arr_ref> <val>
# @DESCRIPTION:
# Checks whether the array reference contains the given value.
# @RETURN: 0 if value exists, 1 otherwise
array_contains() {
    local -n arr=$1
    local val=$2
    shift 2
    if [[ " ${arr[*]} " =~ [[:space:]]${val}[[:space:]] ]]; then
        return 0
    else
        return 1
    fi
}

max_backtrack=10

# @FUNCTION: backtrack
# @USAGE: <map_ref> <start_key> <abort_value>
# @DESCRIPTION:
# Backtrack dependencies through an array list.
# E.g. given an associated array with key value pairs of:
#   B1 -> M
#   B2 -> B1
#   B3 -> B2
#
# ...if we pass B3 as start_key and M as abort_value, then
# we receive the flattened ordered list "B1 B2 B3"
# @STDOUT: space separated list of backtracked values
backtrack() {
    backtrack_ 0 "$1" "$2" "$3"
}

# internal to track backtrack depth
backtrack_() {
    local depth=$1
    if [[ $depth -gt $max_backtrack ]] ; then
        die "Dependency backtracking too deep... aborting!"
    fi
    shift 1

    if [[ $1 != map ]] ; then
      local -n map=$1
    fi

    local base=$2
    local abort_value=$3
    local value

    if [ "${base}" = "${abort_value}" ] ; then
        return
    fi

    value=${map[$base]}

    if [ "${value}" = "${abort_value}" ] ; then
        if ! array_contains branch_list "${base}" ; then
          echo "${base}"
        fi
    else
        if array_contains branch_list "${base}" ; then
          backtrack_ $((depth++)) map "${map[$value]}" "${abort_value}"
        else
          echo "$(backtrack_ $((depth++)) map "${map[$base]}" "${abort_value}")" "${base}"
        fi
    fi
}

create_branch_map() {
    local -n arr=$1
    local -n discovered=$2
    local -n map=$3

	while IFS= read -r branch || [[ -n $branch ]]; do
    rebase_target=$(git branch --merged "${branch}" --sort="ahead-behind:${branch}" --format="%(refname:short)" | grep -e "${branch_regex}" -e '^master$' | awk 'NR==2{print;exit}')

	# this is the case when the branch is actually behind master... we then
	# still want to rebase against master
	if [ -z "${rebase_target}" ] ; then
		rebase_target=master
	fi

    if ! array_contains input_branches "${rebase_target}" && [ "${rebase_target}" != "master" ] ; then
		discovered+=( "${rebase_target}" )
	fi

    map["${branch}"]="${rebase_target}"
done < <(printf '%s\n' "${arr[@]}")
}

{

# create branch rebase tree
# we're doing that on the state of the local tree/master
newly_detected_input_branches=( )
create_branch_map input_branches newly_detected_input_branches branch_map
# these shenanigns are needed in case the rebase target branches themselves do not have
# the 'rebase' label... this would break cherry-picking on master, so we include "parent"
# branches regardless
while true ; do
	if [ ${#newly_detected_input_branches[@]} -eq 0 ] ; then
		break
	else
		nothing=( )
		create_branch_map newly_detected_input_branches nothing branch_map
		newly_detected_input_branches=( "${nothing[@]}" )
	fi
done

} >&2

# flatten recursively
for key in "${!branch_map[@]}"; do
    value=${branch_map[$key]}
    if [ "${value}" = "master" ] ; then
        if ! array_contains branch_list "${key}" ; then
          branch_list+=( "${key}" )
        fi
    else
        # shellcheck disable=SC2207
        branch_list+=( $(backtrack branch_map "$key" "master") )
    fi
done
unset key

result=( )
for key in "${branch_list[@]}"; do
    result+=( "${key}:${branch_map[$key]}" )
done
echo "${result[@]}"

