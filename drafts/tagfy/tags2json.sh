#!/bin/bash

adoc_files=(**/*.adoc)

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

}

##
# For tags in files like:
#
# //
# // tags: foo bar jedi-tux
# //
#
double_forward_slash_tags () {
}

##
# Reads the tags from the provided file.
#
# @params $1 filepath
#
read_tags () {
  printf '[file: %s]\n' "$1"
}

for file in "${adoc_files[@]}"
do
  printf '%s\n' "$file"
  read_tags "$file"
  is_double_forward_slash_tags "$file"
done

