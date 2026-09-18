#!/bin/sh
# Locks the screen. gtklock only loads a stylesheet via --style (no default
# path), so pass ours; config.ini is picked up automatically.
pgrep -x gtklock >/dev/null && exit 0
exec gtklock --style "$HOME/.config/gtklock/style.css"
