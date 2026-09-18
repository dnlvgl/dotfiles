#!/bin/sh
# Prints one line of waybar JSON describing Syncthing's state:
# stopped | error | syncing | disconnected | idle. Needs curl and jq.
# SYNCTHING_URL / SYNCTHING_API_KEY override the defaults (used for testing).

url="${SYNCTHING_URL:-http://127.0.0.1:8384}"
key="${SYNCTHING_API_KEY:-$(grep -oP '(?<=<apikey>)[^<]+' "${XDG_STATE_HOME:-$HOME/.local/state}/syncthing/config.xml" 2>/dev/null)}"

api() { curl -sf -m 2 -H "X-API-Key: $key" "$url/rest/$1"; }

stopped() {
    jq -cn '{text: "", alt: "stopped", class: "stopped", tooltip: "Syncthing: not running"}'
    exit 0
}

folders=$(api config/folders) || stopped
devices=$(api config/devices) || stopped
status=$(api system/status) || stopped
conns=$(api system/connections) || stopped

myid=$(printf '%s' "$status" | jq -r .myID)

tab=$(printf '\t')
folder_state=$(printf '%s' "$folders" | jq -r '.[] | [.id, .paused, (.label // "")] | @tsv' |
    while IFS=$tab read -r id paused label; do
        s=null
        if [ "$paused" != true ]; then
            s=$(api "db/status?folder=$id") || s=null
        fi
        jq -cn --arg id "$id" --arg label "$label" --argjson paused "$paused" --argjson s "$s" '
            {id: $id, label: (if $label == "" then $id else $label end), paused: $paused,
             state: ($s.state // "unknown"), needFiles: ($s.needFiles // 0),
             needBytes: ($s.needBytes // 0), errors: (($s.errors // 0) + ($s.pullErrors // 0))}'
    done | jq -cs .)

peers=$(printf '%s' "$devices" | jq -c --arg me "$myid" --argjson c "$conns" '
    [.[] | select(.deviceID != $me)
     | {id: .deviceID, name: (if .name == "" then .deviceID[0:7] else .name end), paused: .paused,
        connected: ($c.connections[.deviceID].connected // false)}]')

# How far behind each connected peer is; below 100 means we are uploading to it.
peer_state=$(printf '%s' "$peers" | jq -r '.[] | select(.connected and (.paused | not)) | .id' |
    while read -r id; do
        pct=$(api "db/completion?device=$id" | jq '.completion // 100') || pct=100
        jq -cn --arg id "$id" --argjson pct "${pct:-100}" '{id: $id, completion: $pct}'
    done | jq -cs .)

jq -cn --argjson folders "$folder_state" --argjson peers "$peers" --argjson comp "$peer_state" '
    def esc: gsub("&"; "&amp;") | gsub("<"; "&lt;") | gsub(">"; "&gt;");
    def size:
        if . >= 1073741824 then "\(. / 1073741824 * 10 | floor / 10) GiB"
        elif . >= 1048576 then "\(. / 1048576 * 10 | floor / 10) MiB"
        elif . >= 1024 then "\(. / 1024 | floor) KiB"
        else "\(.) B" end;
    ($peers | map(. as $p | . + {completion: ((($comp[] | select(.id == $p.id) | .completion)) // 100)})) as $peers
    | ($folders | map(select(.paused | not))) as $active
    | ($active | any(.state == "error" or .state == "stopped" or .errors > 0)) as $err
    | ($active | any(.state | IN("syncing", "sync-preparing", "sync-waiting"))) as $pulling
    | ($peers | any(.connected and .completion < 100)) as $pushing
    | (if $err then "error"
       elif $pulling or $pushing then "syncing"
       elif ($peers | length) > 0 and ($peers | any(.connected) | not) then "disconnected"
       else "idle" end) as $state
    | ([ "Syncthing: \($state[0:1] | ascii_upcase)\($state[1:])" ]
       + ($folders | map("\(.label | esc): " +
           (if .paused then "paused"
            elif .needFiles > 0 then "\(.state), \(.needFiles) items, \(.needBytes | size) left"
            else .state end) + (if .errors > 0 then " (\(.errors) errors)" else "" end)))
       + ($peers | map("\(.name | esc): " +
           (if .paused then "paused"
            elif .connected then "connected" + (if .completion < 100 then " (\(.completion | floor)%)" else "" end)
            else "disconnected" end)))
       | join("\n")) as $tip
    | {text: "", alt: $state, class: $state, tooltip: $tip}'
