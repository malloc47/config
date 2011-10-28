if [ -f /etc/skel/.bashrc ]; then
	source /etc/skel/.bashrc
fi

# Don't bother if we're running noninteractively
[ -z "$PS1" ] && return

if [ -f /etc/profile.d/bash_completion.sh ]; then
	source /etc/profile.d/bash_completion.sh
fi


# Give me vi control within the terminal
set -o vi
# ^l clear screen
bind -m vi-insert "\C-l":clear-screen

umask 077

# Check for current bash version
if [[ ${BASH_VERSINFO[0]} -ge 4 ]]; then
	shopt -s autocd cdspell
	shopt -s dirspell globstar
fi

# General options
shopt -s cmdhist nocaseglob histappend extglob

# Larger history and ignore common commands
export HISTCONTROL=ignoreboth
export HISTSIZE=5000
export HISTFILESIZE=5000
# export HISTIGNORE="ls*:cd*"

# Workaround for a few games
export SDL_VIDEO_FULLSCREEN_HEAD="1"

# CVS-related variables
export CVS_RSH="ssh"
export CVSROOT=":ext:nyx:/usr/local/cvsroot"

# Include paths for most frequently visited folders
CDPATH=.:~/src/projects/

# Where my scripts live
PATH=$PATH:~/bin:~/src/projects/term-do

# Pager using vim
export PAGER=~/bin/vimpager
alias less=$PAGER

# Use vim as the primary editor
export EDITOR=vim

# To be executed only on lab computers
if [ -d ~/QobiScheme-cvs ] ; then
	export QARCHITECTURE_PATH=`~/bin/architecture-path`
	export ARCHITECTURE_PATH=`~/bin/architecture-path`
	export QINSTALLDIR=~
	export PATH=~/bin/$QARCHITECTURE_PATH:~/local-install/bin:~/bins/bin:$PATH
	export SCMAXHEAP=900
fi


if [ -d ~/darpa-collaboration ] ; then
	export PATH=$PATH:~/darpa-collaboration/bin
#	export LD_LIBRARY_PATH=~/darpa-collaboration/lib:$LD_LIBRARY_PATH
fi

# Multiple-git overlays for my home folder
alias cgit='git --git-dir=$HOME/.cgit'
alias cgits='git --git-dir=$HOME/.cgit status -s'
alias pgit='git --git-dir=$HOME/.pgit'
alias pgits='git --git-dir=$HOME/.pgit status -s'
alias agit='git --git-dir=$HOME/.agit'
alias agits='git --git-dir=$HOME/.agit status -s'

alias ':q'='exit' # Obvious reasons here

alias lsd="ls -al | grep -E '^(d|l)'"

#export TERM="xterm-256color"

source ~/.bashfn

# Source machine-specific files
if [ -f ~/.sys/`hostname`/.bashrc ] ; then
	source ~/.sys/`hostname`/.bashrc 
fi

if [ -d ~/.sys/`hostname`/bin ] ; then
	export PATH=~/.sys/`hostname`/bin:$PATH
fi

if [ "$TERM" != "vt100" -a "$TERM" != "dumb" -a "$EMACS" != "t" ] ; then
	export PS1='\[\e[1;32m\]└─[\[\e[33m\]\h\[\e[1;32m\]:\[\e[1;31m\]$(shorten_path "${PWD/$HOME/~}" 30)\[\e[1;32m\]]/-/\[\e[0m\] '
#Root prompt variant #export PS1="\[\e[1;32m\]└─[\[\e[33m\]\h\[\e[1;32m\]:\[\e[1;31m\]\w\[\e[1;32m\]]/\[\e[1;31m\]-\[\e[1;32m\]/\[\e[0m\] "
	echo -e "\033[1;32m┌─[\033[1;36m`logname`\033[32m@\033[1;33m`hostname -s`\033[1;32m]─[\033[1;31m`pwd`\033[1;32m]─[\033[1;35m`date +%a\ %b\ %d,\ %r`\033[1;32m]"

else
	export PS1="\h [\W]> "
fi
