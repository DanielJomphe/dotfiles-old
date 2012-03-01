#!/bin/bash
#   ~/.bash_profile
#
# Bash Shell login script.

echo ".bash_profile : bash Login       : Sometimes called."

source $HOME/.profile

# What follows is bash-specific stuff.

# (Also compatible with zsh, if I'm ever tempted by it.)
if [ -f `brew --prefix`/etc/autojump ]; then
  . `brew --prefix`/etc/autojump
fi
# Obviously, this is for bash only. (zsh is much better in this area.)
if [ -f `brew --prefix`/etc/bash_completion ]; then
  . `brew --prefix`/etc/bash_completion
fi
