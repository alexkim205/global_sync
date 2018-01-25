# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history
# and don't put lines starting with space.
HISTCONTROL=ignoredups:ignorespace

# basic prompt
export PS1='\[\e[31;1m\]\u\[\e[0m\]: \[\e[35;1m\]\W\[\e[0m\]\$ '

# editor
export EDITOR="emacs -nw"

# aliases & functions

case "$OSTYPE" in
*linux*)
        alias dmesg='dmesg --color'
        alias pacman='pacman --color=auto'
        alias ls='ls --color=auto'
        ;;
*darwin*)
        alias ls='ls -G'
        ;;
esac
alias ll='ls -alF'
alias l='ls -CF'
alias grep="grep --color=auto"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"
alias emacs="emacs -nw"
alias cl="clear"
alias school1="~/Dropbox/Columbia\ University/Freshman/ && ls"
alias school2="~/Dropbox/Columbia\ University/Sophmore/ && ls"

svndiff()     { svn diff "$@" | colordiff; }
svndiffless() { svn diff "$@" | colordiff | less -R; }

alias gccg='gcc -g -Wall'
alias g++g='g++ -g -Wall'
alias valgrindlc='valgrind --leak-check=yes'
