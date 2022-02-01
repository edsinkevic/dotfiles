#!/bin/sh

grep_us_language=$(setxkbmap -print | grep +us+)

if [ "$grep_us_language" != "" ]; then
    setxkbmap lt &
    exit 0
else
    setxkbmap us &
    exit 0
fi

exit 0

