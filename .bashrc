if [ -f /etc/skel/.bashrc ]; then
	source /etc/skel/.bashrc
fi

# Give me vi control within the terminal
set -o vi

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

# Multiple-git overlays for my home folder
alias cgit='git --git-dir=.cgit'
alias pgit='git --git-dir=.pgit'
alias agit='git --git-dir=.agit'

# Where my main scripts live
PATH=$PATH:~/bin/

# To be executed only on lab computers
if [ "${HOSTNAME:(-11)}" == '.cse.sc.edu' ] ; then
	export QARCHITECTURE_PATH=`~/bin/architecture-path`
	export ARCHITECTURE_PATH=`~/bin/architecture-path`
	export QINSTALLDIR=~
	export PATH=~/bin/$QARCHITECTURE_PATH:~/local-install/bin:~/bins/bin:$PATH
	export SCMAXHEAP=900
fi
