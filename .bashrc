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
alias pgit='git --git-dir=$HOME/var/git/personal.git --work-tree=$HOME'
alias agit='git --git-dir=$HOME/var/git/all.git --work-tree=$HOME'
