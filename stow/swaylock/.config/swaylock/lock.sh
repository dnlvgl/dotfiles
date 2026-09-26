#!/bin/sh
# Locks the screen (config: ~/.config/swaylock/config). `daemonize` makes swaylock
# fork once the lock is up, so swayidle's before-sleep hook waits for it.
pgrep -x swaylock >/dev/null && exit 0
exec swaylock
