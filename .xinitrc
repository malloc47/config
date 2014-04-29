#!/bin/sh
xset +fp /usr/share/fonts/local
xset fp rehash
xsetroot -cursor_name left_ptr
urxvtd &
rm ~/.emacs.desktop.lock
emacs --daemon &
[ "`hostname`" != "groupon" ] && xscreensaver -no-splash &
xmodmap .Xmodmap
xset r rate 200 30

if [ "`hostname`" == "groupon" ] ; then
	xrdb -merge ~/.Xresources
	# xrandr -s 2560x1600
	vmware-user-suid-wrapper
fi

feh --bg-scale ~/media/img/wallpaper/current.jpg

exec xmonad
