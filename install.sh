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
link_with_backup .lein

link_with_backup  .emacs-live.el
link_with_backup_2 emacs-live           .emacs.d
link_with_backup_2 emacs-live-packs     .emacs.d.packs
link_with_backup_2 .gitconfig           .config/git/config
link_with_backup_2 .gitignore_global    .config/git/ignore
link_with_backup_2 .git_commit_template .config/git/git_commit_template
