#!/bin/sh
# Idle/sleep inhibitor behind the waybar caffeine module and swayidle's lock gate.
# Usage: caffeine.sh status|json|toggle   (status exits 0 while caffeine is on; json is for waybar)
#
# Kept in a script so `pgrep -f` can't match the calling shell's own command line,
# which contains the pattern whenever it is inlined in an `sh -c` string.
why=waybar-caffeine
pattern="systemd-inhibit --what=idle:sleep --why=$why"

on() { pgrep -f "$pattern" >/dev/null; }

case "$1" in
    status) on ;;
    json)
        if on; then state=on; tip="Caffeine on: no idle lock or sleep"; else state=off; tip="Caffeine off"; fi
        printf '{"alt":"%s","class":"%s","tooltip":"%s"}\n' "$state" "$state" "$tip"
        ;;
    toggle)
        if on; then
            # Also kill the inhibitor's `sleep infinity` child, which would otherwise be orphaned.
            for pid in $(pgrep -f "$pattern"); do
                pkill -P "$pid"
                kill "$pid"
            done
        else
            systemd-inhibit --what=idle:sleep --why=$why sleep infinity >/dev/null 2>&1 &
        fi
        ;;
    *) echo "usage: ${0##*/} status|json|toggle" >&2; exit 2 ;;
esac
