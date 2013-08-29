#!/bin/bash
#   ~/.bash_profile
#
# Bash Shell login script.

echo ".bash_profile : bash Login       : Sometimes called."

source $HOME/.profile

# What follows is bash-specific stuff.

# Add the following line to ~/.zshrc if you use ZSH
[[ -s `brew --prefix`/etc/autojump.sh ]] && . `brew --prefix`/etc/autojump.sh

if [ -f `brew --prefix`/etc/bash_completion ]; then
  . `brew --prefix`/etc/bash_completion
fi
