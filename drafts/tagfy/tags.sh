#!/bin/bash

##
# Checks if tags match syntax like this:
#
#     //
#     // tags: foo bar jedi-tux
#     //
#
# @param $1 filepath
#
is_double_forward_slash_tags () {
	local path="$1"

	local tag_lines=()
	local tgs=no
	local cnt=0

	while read -r line
	do
		if [[ $line =~ ^// ]]
		then
			((cnt++))

			# We should have // tags: on the second line.
			if [[ $line =~ '// tags:' && $cnt = 2 ]]
			then
				echo tgs yes
				tgs=yes
				taglines+=("$line")
			fi

			if [[ $cnt = 3 ]]
			then
				echo 'found 3'
			fi
			printf '%s\n' "$line"
		fi
	done < "$path"

	echo "cnt: $cnt"
	printf '%s\n' "${tag_lines[@]}"
}

is_double_forward_slash_tags "$1"
