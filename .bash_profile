#if [ ! -z $DISPLAY ] ; then
	if command -v keychain &> /dev/null ; then
		eval `keychain -Q -q --eval --agents ssh id_rsa`
	fi
#fi

. $HOME/.bashrc

if [ -f ~/.sys/`hostname`/.bash_profile ] ; then
	source ~/.sys/`hostname`/.bash_profile 
fi

#Emacs ansi-term directory tracking
# track directory, username, and cwd for remote logons
if [ $TERM = eterm-color ]; then
    function eterm-set-cwd {
        $@
        echo -e "\033AnSiTc" $(pwd)
    }
    
    # set hostname, user, and cwd
    function eterm-reset {
        echo -e "\033AnSiTu" $(whoami)
        echo -e "\033AnSiTc" $(pwd)
        echo -e "\033AnSiTh" $(hostname)
    }
    
    for temp in cd pushd popd; do
        alias $temp="eterm-set-cwd $temp"
    done
    
    # set hostname, user, and cwd now
    eterm-reset
fi