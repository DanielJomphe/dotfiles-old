#!/bin/sh
#   ~/.profile
#
# POSIX Shell login script.
#
# Thanks to http://stuff.lhunath.com/.profile

#-------------------------#
# BASE - UTILITY          #
#-------------------------#
exists() {
    test -x "$(type -P "$1" 2>/dev/null)"
}
#exec 2>set-x
#sudo dtruss -p $$ &>dtruss &

#-------------------------#
# BASE - DEV              #
#-------------------------#
export                  DEV_HOME="$HOME/dev"
export                  CLOJURESCRIPT_HOME="$DEV_HOME/clojurescript"

#-------------------------#
# BASE - PATH             #
#-------------------------#
export                  PATH="$HOME/.bin:/usr/local/sbin:/usr/local/bin"
[ -d "/sw" ]         && PATH="$PATH:/sw/sbin:/sw/bin"
[ -d "/opt/local" ]  && PATH="$PATH:/opt/local/sbin:/opt/local/bin"
                        PATH="$PATH:/usr/bin:/bin:/usr/sbin:/sbin"
[ -d "/opt/X11" ]    && PATH="$PATH:/opt/X11/bin"
[ -d "/usr/X11" ]    && PATH="$PATH:/usr/X11/bin"
[ -d "/opt/java" ]   && PATH="$PATH:/opt/java/bin"
[ -d "$EPREFIX" ]    && PATH="$PATH:$EPREFIX/usr/sbin:$EPREFIX/usr/bin:$EPREFIX/sbin:$EPREFIX/bin"
[ -d "$DEV_HOME" ]   && PATH="$PATH:$CLOJURESCRIPT_HOME/bin"
[ -d "/usr/lib/git-core" ] \
                     && PATH="$PATH:/usr/lib/git-core"
[ -d "/Developer/usr/bin" ] \
                     && PATH="$PATH:/Developer/usr/bin"
[ -d "/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin" ] \
                     && PATH="$PATH:/Developer/Platforms/iPhoneOS.platform/Developer/usr/bin"


export                  MANPATH="$HOME/.man:/usr/local/share/man:/usr/local/man"
[ -d "/sw" ]         && MANPATH="$MANPATH:/sw/share/man"
[ -d "/opt/local" ]  && MANPATH="$MANPATH:/opt/local/share/man:/opt/local/man"
[ -d "/opt/X11" ]    && MANPATH="$MANPATH:/opt/X11/share/man"
[ -d "/usr/X11" ]    && MANPATH="$MANPATH:/usr/X11/share/man"
[ -d "$EPREFIX" ]    && MANPATH="$MANPATH:$(source "$EPREFIX/etc/profile.env"; echo "$MANPATH")"
                        MANPATH="$MANPATH:/usr/share/man:/usr/man"

#-------------------------#
# BASE - SECURITY         #
#-------------------------#
#umask 027

#-------------------------#
# BASE - APPLICATIONS     #
#-------------------------#
export EDITOR=$(type -P emacs || type -P vim || type -P vi || type -P nano)

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
#export storia='xx@yy.net'
[ -r "$HOME/.profile.local" ] && \
    . "$HOME/.profile.local"
[ "$BASH_VERSION" -a -z "$POSIXLY_CORRECT" ] && . "$HOME/.bashrc"
