#!/bin/bash
xmodmap ~/.Xmodmap

 
# Load resources
 
xrdb -merge .Xresources
 
# Set up an icon tray
 
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 10 --transparent true --tint 0x191970 --height 12 &
 
# Set the background color
xsetroot -solid midnightblue
 
# Fire up apps
xscreensaver -no-splash &

#Set the wallpaper
xloadimage -center -onroot -fullscreen /home/arjun/Pictures/Wallpapers/write-code.png

#Set the default cursor
xsetroot -cursor_name left_ptr

if [ -x /usr/bin/nm-applet ] ; then
   nm-applet &
fi
 
if [ -x /usr/bin/gnome-power-manager ] ; then
   sleep 3
   gnome-power-manager &
fi

syndaemon -i 1 -t -d 
exec xmonad

