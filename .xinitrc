#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
xset +fp /usr/share/fonts/local
xset fp rehash
xsetroot -cursor_name left_ptr

#trayer --transparent false --widthtype request --height 18 --align right --edge top --expand true &

#trayer --transparent true --widthtype pixel --width 103 --height 8 --align right --edge top --expand true --tint 0x000000 --alpha 0 &

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
