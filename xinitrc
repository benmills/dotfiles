$PATH=$PATH:/usr/local/bin
xsetroot -solid '#4a525a'
xmodmap -e 'remove Mod2 = Meta_L' -e 'add Mod1 = Meta_L'
exec >>~/.xsession-errors 2>&1
/usr/local/bin/dwm-launch
