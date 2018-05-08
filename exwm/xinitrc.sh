#!/bin/sh

# xinitrc for EXWM

# xhost +

auth_agent=/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1

# Set themes, etc
if which gnome-settings-daemon >/dev/null; then
    gnome-settings-daemon &
else
    # Set fallback cursor
    xsetroot -cursor_name left_ptr

    # Overrides
    xset b off &
    #numlockx off                    # Turn off numlock

    # Set keyboard repeat rate
    xset r rate 250 30

    if which udiskie >/dev/null; then
        udiskie --tray &
    fi

    if which dropbox >/dev/null; then
        dropbox &
    fi

    if which ${auth_agent}; then
        ${auth_agent} &
    fi
fi

# If Emacs is started in server mode, `emacsclient` is a convenient way to edit
# files in place (used by e.g. `git commit`)
export VISUAL=emacsclient
export EDITOR="$VISUAL"

# Disable access control
# xhost +SI:localuser:$USER

# Set capslock as ctrl
setxkbmap -layout us -option ctrl:nocaps

exec dbus-launch --exit-with-session emacs \
     --debug-init \
     --eval "(require 'init-exwm-config)" \
     "$@"
