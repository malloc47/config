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
    echo -e "\033AnSiTu" "$LOGNAME" # $LOGNAME is more portable than using whoami.
    echo -e "\033AnSiTc" "$(pwd)"
    if [ $(uname) = "SunOS" ]; then
	    # The -f option does something else on SunOS and is not needed anyway.
       	hostname_options="";
    else
        hostname_options="-f";
    fi
    echo -e "\033AnSiTh" "$(hostname $hostname_options)" # Using the -f option can cause problems on some OSes.
    history -a # Write history to disk.
}
    # Track directory, username, and cwd for remote logons.
if [ "$TERM" = "eterm-color" ]; then
    PROMPT_COMMAND=set-eterm-dir
fi
