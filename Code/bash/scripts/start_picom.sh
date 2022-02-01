#!/bin/sh

CONFIG='/home/edvin/.config/picom/picom.conf'

grep_picom=$(inxi -Gxx | grep picom)

if [ "$grep_picom" != "" ]; then
    killall picom && sleep 1 && picom -f --experimental-backends --backend glx --xrender-sync-fence --config $CONFIG &
    exit 0
else
    picom -f --experimental-backends --backend glx --xrender-sync-fence --config $CONFIG &
    exit 0
fi

exit 0
