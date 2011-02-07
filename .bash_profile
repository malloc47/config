if command -v keychain &> /dev/null
then
	eval `keychain -Q -q --eval --agents ssh id_rsa`
fi
. $HOME/.bashrc
