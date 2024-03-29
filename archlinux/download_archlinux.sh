#!/usr/bin/env bash

#
# Find the latest version at this URL:
#
# https://mirror.ufscar.br/archlinux/iso/
#

base_url='https://mirror.ufscar.br/archlinux/iso/2023.11.01'

files=(
  b2sums.txt
  sha256sums.txt
  archlinux-x86_64.iso.sig
  archlinux-x86_64.iso
)

for file in "${files[@]}"
do
  printf -v url '%s/%s' "$base_url" "$file"
  wget "$url"
done

