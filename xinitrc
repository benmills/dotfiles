#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
# colours: 01:normal 02:white 03:red 04:green 05:yellow 06:blue 07:cyan 08:magenta 09:grey

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# start terminal emulation daemon
/usr/bin/urxvtd -q -o -f

xrdb -load ~/.Xresources
xmodmap ~/.Xmodmap
xcalib ~/Settings/colorprofile.icc

DEFAULT_SESSION=dwm
case $1 in
xfce4)
  exec ck-launch-session dbus-launch startxfce4
	;;
dwm)
  while true; do
    $HOME/.dwm-status
    sleep 5
  done &
  exec ck-launch-session bash -c "dropboxd & xfce4-power-manager & dwm"
	;;
*)
	exec $DEFAULT_SESSION
	;;
esac
