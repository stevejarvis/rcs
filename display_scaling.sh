#!/usr/bin/sh
# Scale non-HiDPI external monitors reasonably. Not perfect, but not half
# bad. Very hacky and specific to Fedora on X11 with my monitors at home.
#
# See https://wiki.archlinux.org/index.php/HiDPI#Multiple_displays
#
# To stop this and clean up, remove the autostart desktop entry at 
# ~/.config/autostart/desktop_scaling.desktop

/usr/bin/xrandr --output eDP1 --scale 1x1 --output DP2-2 --scale 2x2 --pos 2560x0 --output DP2-1 --scale 2x2 --pos 6400x0 --fb 9280x2160 &
