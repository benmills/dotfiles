#!/bin/sh

source_system_xinitrcs() {
  if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
      [ -x "$f" ] && . "$f"
    done
    unset f
  fi
}

setup_colors() {
  xcalib ~/.settings/color.icc
}

setup_background() {
  feh --bg-scale ~/pictures/vintage_chicago.jpg
}

setup_powersave() {
  xset +dpms
  xset dpms 0 0 300
}

setup_keys() {
  keychain ~/.ssh/id_rsa
  [ -f ~/.keychain/$HOSTNAME-sh ] && . ~/.keychain/$HOSTNAME-sh 2>/dev/null
  [ -f ~/.keychain/$HOSTNAME-sh-gpg ] && . ~/.keychain/$HOSTNAME-sh-gpg 2>/dev/null
}

setup_daemons() {
  autocutsel -fork &
  autocutsel -selection PRIMARY -fork &
  syndaemon -t -k -i 2 -d &
  unclutter -noevents &
  xautolock -time 5 -locker slock &
}

setup_keyboard() {
  xmodmap ~/.Xmodmap
  xbindkeys
}

setup_terminal() {
  urxvtd -q -o -f
  xrdb -load ~/.Xresources
}

run_dwm() {
  setup_background
  dwm-status
  exec ck-launch-session dbus-launch bash -c "dwm"
}

run_bspwm() {
  setup_background
  [ -e "$PANEL_FIFO" ] && rm "$PANEL_FIFO"
  mkfifo "$PANEL_FIFO"

  sxhkd &
  conky | panel -ta r &
  bspwm-status | panel -w 500 -ta l &

  exec bspwm -s "$PANEL_FIFO"
}

run_herbstluftwm() {
  exec herbstluftwm
}

source_system_xinitrcs
setup_colors
setup_powersave
setup_keys
setup_daemons
setup_keyboard
setup_terminal

case $1 in
  dwm) run_dwm;;
  bspwm) run_bspwm;;
  herb) run_herbstluftwm;;
  *) run_dwm;;
esac

