#!/bin/sh
xset +fp /usr/share/fonts/local
xset fp rehash
xsetroot -cursor_name left_ptr

xmodmap .Xmodmap
xset r rate 200 30
xrdb -merge ~/.Xdefaults
command -v xbindkeys && xbindkeys

if [ "`hostname`" == "rally" ] ; then
  xrandr --dpi 227
  vmware-user-suid-wrapper
else
  xrandr --dpi 109
fi

[ "`hostname`" != "rally" ] && command -v xscreensaver && xscreensaver -no-splash &

command -v urxvtd && urxvtd &

rm ~/.emacs.d/.emacs.desktop.lock
command -v emacs && emacs --daemon &


command -v feh && feh --bg-scale ~/media/img/wallpaper/current.jpg

exec xmonad
