#!/usr/bin/env bash

files=(
	archlinux-x86_64.iso
	archlinux-x86_64.iso.sig
	b2sums.txt
	release-key.pgp
	sha256sums.txt
)

rm -v "${files[@]}"
