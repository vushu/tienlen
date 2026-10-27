#!/bin/sh
# TienLen local runner.
#
# Usage:
#   ./run.sh                      live room on :8080 (grace periods,
#                                 watchdog, seats freed on disconnect)
#   ./run.sh family               persistent room on :8081 (token-owned
#                                 seats, survives restarts) - state in
#                                 ./rooms/family/
#   ./run.sh friends              another persistent room on :8082 -
#                                 state in ./rooms/friends/
#   ./run.sh <name> <port>        persistent room on a custom port
#
# Persistent rooms each get their own directory (rooms/<name>/) so their
# saved games never clobber each other. The room code (if you want one)
# is TIENLEN_ROOM_CODE; the admin key is TIENLEN_ADMIN_KEY.

set -e
cd "$(dirname "$0")"

NAME="${1:-}"
PORT="${2:-}"

if [ -z "$NAME" ]; then
    # Live room: the classic behaviour.
    exec swipl -l server.pl -g start
fi

# Persistent room: default ports per name, or sequential if unknown.
if [ -z "$PORT" ]; then
    case "$NAME" in
        family)  PORT=8081 ;;
        friends) PORT=8082 ;;
        *)       PORT=8083 ;;
    esac
fi

# Each persistent room gets its own state directory.
mkdir -p "rooms/$NAME"

echo "Persistent room '$NAME' on port $PORT (state: rooms/$NAME/)"
cd "rooms/$NAME"
exec env TIENLEN_PERSISTENT=1 TIENLEN_PORT="$PORT" \
    swipl -l ../../server.pl -g start
