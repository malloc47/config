#if [ ! -z $DISPLAY ] ; then
	if command -v keychain &> /dev/null ; then
		eval `keychain -Q -q --eval --agents ssh id_rsa`
	fi
#fi

. $HOME/.bashrc

if [ -f ~/.sys/`hostname`/.bash_profile ] ; then
	source ~/.sys/`hostname`/.bash_profile 
fi
