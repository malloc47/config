#!/bin/sh
xset +fp /usr/share/fonts/local
xset fp rehash
xsetroot -cursor_name left_ptr
command -v urxvtd && urxvtd &
rm ~/.emacs.d/.emacs.desktop.lock
command -v emacs && emacs --daemon &
command -v xscreensaver && xscreensaver -no-splash &
xmodmap .Xmodmap
xset r rate 200 30
xrdb -merge ~/.Xdefaults
command -v xbindkeys && xbindkeys
command -v feh && feh --bg-scale ~/media/img/wallpaper/current.jpg

exec xmonad
