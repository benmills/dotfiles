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

keychain ~/.ssh/id_rsa
[ -f ~/.keychain/$HOSTNAME-sh ] && . ~/.keychain/$HOSTNAME-sh 2>/dev/null
[ -f ~/.keychain/$HOSTNAME-sh-gpg ] && . ~/.keychain/$HOSTNAME-sh-gpg 2>/dev/null

# start terminal emulation daemon
/usr/bin/urxvtd -q -o -f

autocutsel -fork &
autocutsel -selection PRIMARY -fork &
syndaemon -t -k -i 2 -d &

# hide cursor
unclutter -noevents &

xrdb -load ~/.Xresources
xmodmap ~/.Xmodmap
xbindkeys
xcalib ~/.settings/color.icc

dropboxd &
xautolock -time 5 -locker slock &
sudo /home/benmills/.bin/keyboard_brightness_setter &

feh --bg-scale ~/pictures/SUFj9.png

while true; do
  $HOME/.bin/dwm-status
  sleep 2
done &

exec ck-launch-session dbus-launch bash -c "dwm"
