#!/bin/bash
#   ~/.bashrc
#
# Bash Shell initialization script.
#
# Thanks to http://stuff.lhunath.com/.bashrc

echo ".bashrc       : bash Interactive : Always called."

source $HOME/.mypath

#-------------------------#
# SHELL - INITIALIZATION  #
#-------------------------#
[[ $PATH = *local/bin* ]] || PATH=$PATH:/usr/local/bin
[[ $- = *i* ]] || return
source $HOME/bashlib  # http://stuff.lhunath.com/bashlib

#-------------------------#
# ALIASSES - FILESYSTEM   #
#-------------------------#
alias noproxy="http_proxy= HTTP_PROXY= https_proxy= HTTPS_PROXY= ftp_proxy= FTP_PROXY="
alias rs="rsync --archive --no-owner --verbose --sparse --hard-links --partial --progress"
alias rsz="rs --compress-level=9 --skip-compress=gz/zip/z/rpm/deb/iso/bz2/t[gb]z/7z/mp[34]/mov/avi/ogg/jpg/jpeg/rar/gif/png/dat"
alias mvn="nice mvn"
alias lein="nice lein"
alias cp="cp -v"
alias mv="mv -v"
alias tree="tree -CF --dirsfirst"
if ls --color >/dev/null 2>/dev/null; then
    alias ls="ls -bFk --color=auto"
else
    alias ls="ls -bFkG"
fi
alias ll="ls -lh"
alias l=" ll -A"
alias df="df -h"

alias h?="history | grep"
alias b?="cat $HOME/.bashrc  | grep"
alias p?="cat $HOME/.profile | grep"

#-------------------------#
# ALIASSES - SYSTEM       #
#-------------------------#
s() {
    fc -s "$1=$2"
}
exists wdiff && \
    alias wdiff="wdiff -a"
exists less && \
    alias more="less" || \
    alias less="more"
alias kbg='bg; kill $!; fg'
exists ltrace && \
    alias trace="ltrace -C -f -n 4 -S"
exists pcregrep && \
    alias pcregrep="pcregrep --color=auto"
alias grep="grep -I --color=auto"
alias egrep="grep -E"
alias fgrep="grep -F"
alias pg="pgrep -l"
if exists pstree; then
    p() {
        if pstree -A >/dev/null 2>&1; then
            pstree -Aahlpu
        else
            [[ -t 1 ]] \
                && pstree -w -g2 \
                || pstree -w -g1 | recode -fg IBM850..UTF8
        fi
    }
else
    if ps auxf >/dev/null 2>&1; then
        p() { ps auxf; }
    else
        p() { ps aux; }
    fi
fi
alias pp="p | less"
top -u -h >/dev/null 2>&1 && \
    alias top="top -S -u -stats pid,ppid,user,cpu,time,threads,state,rprvt,vprvt,faults,command"

#-------------------------#
# ALIASSES - NETWORKING   #
#-------------------------#
alias n="netstat -np tcp"
alias mtr="mtr -t"
alias nmap="nmap -v -v -T5"
alias nmapp="nmap -P0 -A --osscan_limit"
alias pktstat="sudo pktstat -tBFT"

#-------------------------#
# OS-SPECIFIC             #
#-------------------------#
if [[ $MACHTYPE = *darwin* ]]; then
    # MAC ----------------#

    qview() {
        files=("$@"); i=0
        while true; do
            file=${files[i]}
            qlmanage -p "$file" & pid=$!

            read -sn1 key
            kill $pid || key=q
            wait $pid

            case $key in
                q)  return  ;;
                p)  let i-- ;;
                *)  let i++ ;;
            esac

            (( i < ${#files[@]} )) || break
            (( i < 0 )) && i=0
        done
    } 2>/dev/null

    qthumb() {
        qlmanage -t "$@" & pid=$!
        read -sn1

        kill $pid; wait $pid
    } 2>/dev/null

    alias cwd="pwd | pbcopy" # copies pwd to clipboard
    alias gowd='cd "`pbpaste`"' # opens pasted dir in new window
    alias preview='groff -Tps > /tmp/tmp.ps && open -a Preview /tmp/tmp.ps' # pipe to preview

    cdf () { # cd to the dir shown in top-most Finder window # I may have broken this one; let's google it if that's the case.
       currFolderPath=$( /usr/bin/osascript <<<
           tell application "Finder"
               try
                   set currFolder to (folder of the front window as alias)
               on error
                   set currFolder to (path to desktop folder as alias)
               end try
               POSIX path of currFolder
           end tell
       )
       echo "cd to \"$currFolderPath\""
       cd "$currFolderPath"
    }
fi

#-----------------------------#
# SHELL - HISTORY             #
#-----------------------------#
shopt -s histappend
HISTCONTROL=ignoreboth
HISTSIZE=
HISTFILESIZE=
HISTTIMEFORMAT='%F %T%t'

#-------------------------#
# SHELL - LOOK AND FEEL   #
#-------------------------#
{
shopt -s extglob
shopt -s globstar
shopt -s checkwinsize
shopt -s hostcomplete
shopt -s no_empty_cmd_completion
stty stop undef
stty -echoctl
} 2>/dev/null

#[[ -f /etc/bash_completion ]] && \
#    source /etc/bash_completion

# Terminal title for xterm/rxvt/screen.
case "$SHELL" in
    */bash|bash)
        case "$TERM" in
            ?term*|rxvt*|gnome*|interix)
                PROMPT_COMMAND='history -a; echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}\007"'   ;;
            screen*)
                PROMPT_COMMAND='history -a; echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}\033\\"'   ;;
        esac
    ;;
esac

# Prompt
PS1='\h\[$blue\] \W \[$red\]${?/#0/\[$green\]}\$\[$reset\] '
if (( EUID )); then
    PS1='\[$reset$bold$green\]\u@'$PS1
else
    PS1='\[$reset$bold$red\]'$PS1
fi
#PS1='\[$reset$bold$green\]\u@\h\[$blue\] \W \[$green\]$(: "${?#0}"; printf "\[%s\]" "${_:+$save$red\r$_ $load}")\$\[$reset\] '

# Colors
if      [[ -f /etc/DIR_COLORS.env ]]
then        source "/etc/DIR_COLORS.env"
elif    [[ -f /etc/DIR_COLORS ]] && exists dircolors
then        eval "$(dircolors -b "/etc/DIR_COLORS")"
fi
if      [[ -f $HOME/dir_colors.env ]]
then        source "$HOME/dir_colors.env"
elif    [[ -f $HOME/dir_colors ]] && exists dircolors
then        eval "$(dircolors -b "$HOME/dir_colors")"
fi

# X Resources.
#[ "$DISPLAY" -a -f "$HOME/.Xdefaults" ] && \
#    exists xrdb && xrdb "$HOME/.Xdefaults"

#-------------------------#
# FUNCTIONS - CONVENIENCE #
#-------------------------#
i() {
    bash --rcfile <(printf '%s\n' "$(<~/.bashrc)" "HISTFILE='$HISTFILE.i'" "PS1='\[$yellow\]~\[$reset\]$PS1'" "cd; clear"); clear
}
d() {
    if exists colordiff; then
        colordiff -ur "$@"
    elif exists diff; then
        diff -ur "$@"
    elif exists comm; then
        comm -3 "$1" "$2"
    fi | less
}
rerun() {
    local h history histories dialogMenu=() startIndex

    # Load in history entries (ignoring the last)
    IFS=$'\n' read -r -d '' -a histories < <(history | tail -n "${1:-10}")
    unset histories[${#histories[@]}-1]

    # Build a dialog(1) menu and show it to ask for the starting index.
    for h in "${!histories[@]}"; do dialogMenu+=( "$h" "${histories[h]}" ); done
    startIndex=$(dialog --stdout --menu "Bash History" 0 0 0 "${dialogMenu[@]}")

    # Run all history commands starting from the starting index.
    for history in "${histories[@]:startIndex}"; do
        eval "${history#*$'\t'}" || break
    done
}
sw() {
    local bak="$1~"
    while [[ -e $bak ]]
    do bak+='~'; done
    mv -v "$1" "$bak"
    mv -v "$2" "$1"
    mv -v "$bak" "$2"
}
ppg() {
    pat=$1; shift
    p | grep -i "$@" "$pat"
}
cwatch() {
    while sleep .5; do
        o="$("$@")"
        clear && echo "$o"
    done
}
mvnroot() {
    local p=$PWD c=${1:-1}
    until p=${p%/*}; [[ -e "$p/pom.xml" ]] && (( --c <= 0 )); do :; done

    echo "${p}${red}${PWD#$p}${reset}"
    cd "$p"
}
gf() {
    git-forest --all -a --sha "$@" | less
}
gdm() {
    emit "GIT Daemon starting with base path: $(shorten "$PWD")"
    git daemon --base-path=. "$@" &
}

#-------------------------#
# FUNCTIONS - NETWORKING  #
#-------------------------#
exists lft && \
    lft() {
        sudo lft -S "$@" | tail -n +3 | column -t
    }
svnup() {
    local cRev=$(svn info | awk '/^Revision:/ { print $2 }')
    [[ $cRev ]] || { emit -r "Not in a repository."; return 1; }

    emit "Looking for updates to r$cRev"
    svn up

    local nRev=$(svn info | awk '/^Revision:/ { print $2 }')
    [[ $nRev = $cRev ]] && {
        emit "Nothing to update."
        return
    }

    echo
    emit "Changelog $cRev -> $nRev:"
    svn log -r$((cRev+1)):$nRev | while IFS= read -r line; do
        [[ $line ]] || continue

        [[ $line != *[^-]* ]] && { begin=1; continue; }
        if (( begin )); then
            read rev _ user _ d t _ <<< "$line"
            echo
            echo "$bold$green$user$reset - r$bold${rev#r}$reset:    "$'\t'"($bold$d$reset - $bold$t$reset)"
            begin=0
        else
            [[ $line = *:* ]] \
                && echo $'\t'"$reset- $bold$blue${line%%:*}$reset:${line#*:}" \
                || echo $'\t'"$reset- $line"
        fi
    done

    echo
    emit "To view the code diff of these updates; execute: svn diff -r$cRev:$nRev $(quote "$@")"
}

#-------------------------#
# FUNCTIONS - FILE SYSTEM #
#-------------------------#
cc() {
    [[ $@ =~ ^\ *(.*)\ +([^\ ]+)\ *$ ]] && \
        tar -Sc ${BASH_REMATCH[1]} | \
            tar --preserve -xvC ${BASH_REMATCH[2]}
}
md() { mkdir -p "$@" && cd "$@"; } # create a new (nested) dir and enter it

#-------------------------#
# FUNCTIONS - EVALUATION  #
#-------------------------#
calc() { python -c "import math; print $*"; }
c() {
    local out="${TMPDIR:-/tmp}/c.$$" strict=1
    trap 'rm -f "$out"' RETURN

    [[ $1 = -l ]] && { strict=; shift; }

    local code="
#include <stdio.h>
#include <math.h>

int main(int argc, const char* argv[]) {
    $1;
    return 0;
}"
    shift

    if ! gcc -x c -o "$out" -Wall ${strict:+-Werror} - <<< "$code"; then
        emit -r "Compilation failed:"
        cat -n <<< "$code"
        return 255
    else
        chmod +x "$out" && "$out" "$@"
    fi
}

#-------------------------#
# STARTUP APPLICATIONS    #
#-------------------------#

# PYTHON

# http://hackercodex.com/guide/python-virtualenv-on-mac-osx-mountain-lion-10.8/
# pip should only run if there is a virtualenv currently activated
export PIP_REQUIRE_VIRTUALENV=true
# cache pip-installed packages to avoid re-downloading
export PIP_DOWNLOAD_CACHE=$HOME/.pip/cache
# and provide `syspip` for when times call for a global pip call
syspip(){
   PIP_REQUIRE_VIRTUALENV="" pip "$@"
}
alias syspip-upgrade="syspip install --upgrade pip setuptools virtualenv"
