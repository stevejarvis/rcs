#!/usr/bin/sh

# Scale non-HiDPI external monitors reasonably. Not perfect, but not half
# bad. Very hacky and specific to Fedora on X11 with my monitors at home.
#
# See https://wiki.archlinux.org/index.php/HiDPI#Multiple_displays
#
# To start/stop this automatically, make autostart desktop entry at 
# ~/.config/autostart/desktop_scaling.desktop
function docked-tpad {
    /usr/bin/xrandr --output eDP1 --scale 1x1 --output DP2-2 --scale 2x2 --pos 2560x0 --output DP2-1 --scale 2x2 --pos 6400x0 --fb 9280x2160
}

function undocked-tpad {
    /usr/bin/xrandr --output eDP1 --scale 1x1 
}

# utility function for cleaning up unnamed docker images
function docker_rmia {
    docker rmi $(docker images | grep '^<none>' | awk '{print $3}')
}

# utility function delete all current containers
function docker_rma {
    docker rm $(docker ps -a -q)
}


