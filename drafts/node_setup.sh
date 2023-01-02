#!/usr/bin/env bash

echo "SHELL: $SHELL"

##
# Figure out the user's default shell.
#
shell="${SHELL##*/}"
shellrc="${HOME}/.${shell}rc"
version_nvm='v0.39.3'

##
# Create or fix ZSH configuration for user.
#
mkzshrc () {
  local bkp="$HOME/.zshrc_$(date +'%Y-%m-%d_%M:%S').bkp"
  local rc="$HOME/.zshrc"


  if [ ! -r "$rc" ] ; then
    printf '\n'> "$rc"
  fi

  ##
  # If all setup, we can bail from this function.
  #
  if grep -q ZSH_DISABLE_COMPFIX "$rc" ; then
    return 0
  fi

  mv "$rc" "$bkp"

  sed -f - "$bkp" <<EOF > "$rc"
1 i\\
ZSH_DISABLE_COMPFIX=true\\

EOF
}

if [ "$shell" = zsh ] ; then
  mkzshrc
elif [ "$shell" = bash ] ; then
  touch ~/.bashrc
fi

exit 0

##
# Install nvm.
#
curl -o- \
  "https://raw.githubusercontent.com/nvm-sh/nvm/${version_nvm}/install.sh" \
  | bash

##
# We need nvm (Node Version Manager) for this script to work.
# Let's check if the installation worked.
#
# shellcheck source=/dev/null
if [ ! -r ~/.nvm/nvm.sh ]
then
  cat <<EOF

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!                                                        !!!
    !!!      PLEASE INSTALL NVM AND RUN THIS SCRIPT AGAIN.     !!!
    !!!                                                        !!!
    !!!  https://github.com/nvm-sh/nvm#installing-and-updating !!!
    !!!                                                        !!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

EOF
exit 1
else
  source ~/.nvm/nvm.sh
fi

##
# @param $1 The name of the key to parse the version from ‘package.json’.
# Must be either ‘node’ or ‘npm’.
#
parse_version () {
  key="$1"
  printf '%s' \
    "$(\
    sed -n 's/^ *"'"$key"'": "\([^"]*\).*/\1/p' \
    < package.json \
    )"
  }

##
# The version of node and npm as set in the ‘engines’ field in
# package.json (not including the patch).
#
# Example node_version: 16.8
# Example npm version: 8.2
#
version_node=$(parse_version node)
version_npm=$(parse_version npm)

if ! node --version | grep -q "$version_node"
then
  nvm install < .nvmrc
fi

##
# Refresh the shell's environment so it picks up where node
# has been installed.
#
source "$shellrc"

##
# Set up the npm command.
#
npm_program="$HOME/.nvm/versions/node/v${version_node}/bin/npm"

##
# Finally install the `npm` version specified in package.json.
#
if ! npm --version | grep -q "$version_npm"
then
  command "$npm_program" install --global "npm@$version_npm"
fi

cat <<EOF

    ======================================
    === 💯 Great! You are all set!! 💯 ===
    ======================================

    • [✔] node: ---: $(node --version | sed 's/^v//')
    • [✔] npm -----: $(npm --version)

EOF
