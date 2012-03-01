#!/bin/sh
#   ~/.profile
#
# POSIX Shell login script.
#
# Thanks to http://stuff.lhunath.com/.profile

echo ".profile      :  *sh Login       : Sometimes called."

#-------------------------#
# BASE - UTILITY          #
#-------------------------#
exists() {
    test -x "$(type -P "$1" 2>/dev/null)"
}

#-------------------------#
# BASE - SECURITY         #
#-------------------------#
#umask 027

#-------------------------#
# BASE - APPLICATIONS     #
#-------------------------#
#export ALTERNATE_EDITOR=emacs
export EDITOR=$(type -P emacsclient || type -P emacs || type -P vim || type -P vi || type -P nano)
#export VISUAL=$(type -P emacsclient || type -P emacs || type -P vim || type -P vi || type -P nano)
#alias emacs="emacsclient -n"

#-------------------------#
# ENVIRONMENT - LOCALE    #
#-------------------------#
LANG=$(printf '%s\n' "$(locale -a)" en_{US,CA}{.utf8,.UTF-8,.iso88591,.ISO-8859-1,} C 2>/dev/null | awk 'a[$0]++{print;exit}')
export LANG LC_COLLATE="C" TZ=Canada/Eastern

#-------------------------#
# SHELL - CHECK TYPE      #
#-------------------------#
[[ $- != *i* ]] && return

#-----------------------------#
# ENVIRONMENT - APPLICATIONS  #
#-----------------------------#
exists lesspipe.sh && \
    export LESSOPEN="|lesspipe.sh %s"
export LESS="-i -M -R -W -S"
export GREP_COLOR=31
export NOSPLASH=1
export NOWELCOME=1
export PAGER=less
export MANPAGER=$PAGER

#-----------------------------#
# ENVIRONMENT - LOCAL CONFIG  #
#-----------------------------#
#source $HOME/.mypath

[ -r "$HOME/.profile.local" ] && \
    source "$HOME/.profile.local"

[ "$BASH_VERSION" -a -z "$POSIXLY_CORRECT" ] && source "$HOME/.bashrc"

