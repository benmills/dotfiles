#!/bin/sh

source_system_xinitrcs() {
  if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
      [ -x "$f" ] && . "$f"
    done
    unset f
  fi
}

setup_background() {
  feh --bg-center ~/pictures/Einn.jpg
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
  xrdb -load ~/.Xresources
}

run_dwm() {
  while true; do
    $HOME/.bin/dwm-status
    sleep 2
  done &

  exec dbus-launch bash -c "dwm"
}

source_system_xinitrcs
setup_powersave
setup_daemons
setup_keyboard
setup_terminal
setup_background
setup_keys

run_dwm
