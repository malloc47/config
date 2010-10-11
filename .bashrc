if [ -f /etc/skel/.bashrc ]; then
	source /etc/skel/.bashrc
fi

export HISTCONTROL=ignoreboth
export HISTSIZE=2000
export HISTFILESIZE=2000
export HISTIGNORE="ls*:cd*"

export SDL_VIDEO_FULLSCREEN_HEAD="1"

export CVS_RSH="ssh"
export CVSROOT=":ext:nyx:/usr/local/shape/cvsroot"

#alias cgit='git --git-dir=$HOME/var/git/config.git --work-tree=$HOME'
alias cgit='git --git-dir=.cgit'
alias pgit='git --git-dir=.pgit'
alias agit='git --git-dir=.agit'

PATH=$PATH:~/bin/

if [ "${HOSTNAME:(-11)}" == '.cse.sc.edu' ] ; then
	export QARCHITECTURE_PATH=`~/bin/architecture-path`
	export ARCHITECTURE_PATH=`~/bin/architecture-path`
	export QINSTALLDIR=~
	export PATH=~/bin/$QARCHITECTURE_PATH:~/local-install/bin:~/bins/bin:$PATH
	export SCMAXHEAP=900
fi
