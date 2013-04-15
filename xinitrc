#!/bin/sh

source_system_xinitrcs() {
  if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
      [ -x "$f" ] && . "$f"
    done
    unset f
  fi
}

setup_visual_env() {
  feh --bg-scale ~/Pictures/sky2.jpg
  xcalib ~/.settings/color.icc
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
  while true; do
    $HOME/.bin/dwm-status
    sleep 2
  done &

  exec ck-launch-session dbus-launch bash -c "dwm"
}

run_bspwm() {
  sxhkd &

  [ -e "$PANEL_FIFO" ] && rm "$PANEL_FIFO"
  mkfifo "$PANEL_FIFO"

  exec bspwm -s "$PANEL_FIFO"
}

source_system_xinitrcs
setup_visual_env
setup_powersave
setup_keys
setup_daemons
setup_keyboard
setup_terminal

case $1 in
  dwm) run_dwm;;
  bspwm) run_bspwm;;
  *) run_dwm;;
esac

