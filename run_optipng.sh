#!/usr/bin/env bash

#
# Runs optipng on new or changed .png images.
# Assumes git, optipng and coreutils are installed.
#

imgs=(\
	$(git status --short --porcelain \
	| grep '\.png$' \
	| cut -d ' ' -f 2\
	)\
)

for img in "${imgs[@]}"
do
	optipng -o7 "$img"
done
