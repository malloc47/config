if [ -f /etc/skel/.bashrc ]; then
	source /etc/skel/.bashrc
fi

if [ -f /etc/profile.d/bash_completion.sh ]; then
	source /etc/profile.d/bash_completion.sh
fi


# Give me vi control within the terminal
set -o vi
# ^l clear screen
bind -m vi-insert "\C-l":clear-screen


# Check for current bash version
if [[ ${BASH_VERSINFO[0]} -ge 4 ]]; then
	shopt -s autocd cdspell
	shopt -s dirspell globstar
fi

# General options
shopt -s cmdhist nocaseglob
shopt -s histappend extglob

# Larger history and ignore common commands
export HISTCONTROL=ignoreboth
export HISTSIZE=5000
export HISTFILESIZE=5000
export HISTIGNORE="ls*:cd*"

# Workaround for a few games
export SDL_VIDEO_FULLSCREEN_HEAD="1"

# CVS-related variables
export CVS_RSH="ssh"
export CVSROOT=":ext:nyx:/usr/local/shape/cvsroot"

# Include paths for most frequently visited folders
CDPATH=.:~/src/projects/

# Where my scripts live
PATH=$PATH:~/bin/

# Pager using vim
export PAGER=~/bin/vimpager
alias less=$PAGER

# Use vim as the primary editor
export EDITOR=vim

# To be executed only on lab computers
if [ "${HOSTNAME:(-11)}" == '.cse.sc.edu' ] ; then
	export QARCHITECTURE_PATH=`~/bin/architecture-path`
	export ARCHITECTURE_PATH=`~/bin/architecture-path`
	export QINSTALLDIR=~
	export PATH=~/bin/$QARCHITECTURE_PATH:~/local-install/bin:~/bins/bin:$PATH
	export SCMAXHEAP=900
fi

# Multiple-git overlays for my home folder
alias cgit='git --git-dir=.cgit'
alias cgits='git --git-dir=.cgit status -s'
alias pgit='git --git-dir=.pgit'
alias pgits='git --git-dir=.pgit status -s'
alias agit='git --git-dir=.agit'
alias agits='git --git-dir=.agit status -s'

alias ':q'='exit' # Obvious reasons here

# Ls after CD
cd() { if [[ -n "$1" ]]; then builtin cd "$1" && ls;
else builtin cd && ls; fi; }
,cd() { [[ -n "$1" ]] && builtin cd "$1" || builtin cd; }

# Going up directories
function ..(){ for ((j=${1:-1},i=0;i<j;i++));do builtin cd ..;done;}
alias ...='.. 2'
alias ....='.. 3'
alias .....='.. 4'
alias .......='.. 5'

alias xkcd='feh `lynx --dump http://xkcd.com/| grep png`'
