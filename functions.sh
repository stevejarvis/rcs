#!/usr/bin/sh

# Scale non-HiDPI external monitors reasonably. Not perfect, but not half
# bad. Very hacky and specific to Fedora on X11 with my monitors at home.
#
# See https://wiki.archlinux.org/index.php/HiDPI#Multiple_displays
#
# To start/stop this automatically, make autostart desktop entry at 
# ~/.config/autostart/desktop_scaling.desktop
#function docked_tpad {
#    /usr/bin/xrandr --output eDP1 --scale 1x1 --output DP2-2 --scale 2x2 --pos 2560x0 --fb 6400x2160
#}

function docked_tpad {
    # Scale doubles the pixel count.
    # DELL = 1920x1080, Samsung = 1440x900
    DELL="DP2-2"
    SAMSUNG="DP2-3"
    BUILTIN="eDP1"
    /usr/bin/xrandr --output $SAMSUNG --scale 2x2 --pos 0x0 --output $DELL --scale 2x2 --pos 2880x0 --output $BUILTIN --scale 1x1 --pos 6720x0 --fb 9280x2160
}

function undocked_tpad {
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


