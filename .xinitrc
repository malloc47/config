#!/bin/sh
xset +fp /usr/share/fonts/local
xset fp rehash
xsetroot -cursor_name left_ptr
urxvtd &
rm ~/.emacs.d/.emacs.desktop.lock
emacs --daemon &
[ "`hostname`" != "groupon" ] && xscreensaver -no-splash &
xmodmap .Xmodmap
xset r rate 200 30
xrdb -merge ~/.Xdefaults

if [ "`hostname`" == "groupon" ] ; then
	# xrandr -s 2560x1600
	vmware-user-suid-wrapper
fi

feh --bg-scale ~/media/img/wallpaper/current.jpg

exec xmonad
