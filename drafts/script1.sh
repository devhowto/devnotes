#!/bin/bash

while read -r line
do
	printf '%b %s\n' '\u2022' "$line"
done <<< $'one\ntwo\nthree\n'
