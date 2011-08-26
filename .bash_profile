#if [ ! -z $DISPLAY ] ; then
	if command -v keychain &> /dev/null ; then
		eval `keychain -Q -q --eval --agents ssh id_rsa`
	fi
#fi

. $HOME/.bashrc

if [ -f ~/.sys/`hostname`/.bash_profile ] ; then
	source ~/.sys/`hostname`/.bash_profile 
fi

function set-eterm-dir {
    echo -e "\033AnSiTu" "$LOGNAME"
    echo -e "\033AnSiTc" "$(pwd)"
    echo -e "\033AnSiTh" "$(hostname -s)" # Works better than the
					  # fully-qualified name when
					  # dealing with odd port
					  # numbers
   # history -a # Write history to disk.
}
    # Track directory, username, and cwd for remote logons.
if [ "$TERM" = "eterm-color" ]; then
    PROMPT_COMMAND=set-eterm-dir
fi
