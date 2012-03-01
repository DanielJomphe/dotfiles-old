#!/usr/bin/env bash

set -e

cd `dirname $0`
export DOTFILES=`pwd`
source $DOTFILES/install_functions.sh

link_with_backup bashlib
link_with_backup .bashrc
link_with_backup .bash_profile
link_with_backup .profile
link_with_backup .mypath
link_with_backup .gitconfig
link_with_backup .gitignore_global
link_with_backup .git_commit_template

link_with_backup .emacs.d
