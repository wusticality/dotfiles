#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then







    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then







    xrdb -merge "$userresources"

fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# Set your monitor layout.
xrandr --output DP-2 --mode 3840x2160 --rate 144 --output DP-4 --primary --mode 3840x2160 --rate 144 --right-of DP-2 --output DP-0 --mode 3840x2160 --rate 144 --right-of DP-4

# Set your keyboard options.
setxkbmap -option altwin:swap_alt_win,caps:none

# Set your keyboard repeat speed.
xset r rate 264 48

# Set your compositor.
picom &

# Start xmonad.
exec xmonad
