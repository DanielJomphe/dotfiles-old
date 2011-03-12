if [ -f `brew --prefix`/etc/autojump ]; then
  . `brew --prefix`/etc/autojump
fi

#export EDITOR=/usr/local/bin/emacs


alias "."="pwd"
alias ".."="cd .."
alias "..."="cd ../.."

alias "ls=ls -G"
alias "la=ls -Gla"
alias "ll=ls -Gl"

md() { mkdir -p "$@" && cd "$@"; } # create a new (nested) dir and enter it

alias grep="grep --color=auto"


### OS X
alias cwd="pwd | pbcopy" # copies pwd to clipboard
alias gowd='cd "`pbpaste`"' # opens pasted dir in new window

cdf () { # cd to the dir shown in top-most Finder window
   currFolderPath=$( /usr/bin/osascript <<"         EOT"
       tell application "Finder"
           try
               set currFolder to (folder of the front window as alias)
           on error
               set currFolder to (path to desktop folder as alias)
           end try
           POSIX path of currFolder
       end tell
         EOT
   )
   echo "cd to \"$currFolderPath\""
   cd "$currFolderPath"
}

alias "ql=qlmanage -p 2>/dev/null" # ql file
alias preview='groff -Tps > /tmp/tmp.ps && open -a Preview /tmp/tmp.ps' # pipe to preview 