# TienLen Server Setup

A Prolog implementation of the Vietnamese card game Tiến Lên, with a
WebSocket game server, a web client, and an admin page.

## Quick start (local)

```sh
./run          # or ./run.sh — foreground on http://localhost:8080
```

Open [http://localhost:8080](http://localhost:8080) in one tab per player
(2–4 players). Press **New Game** once at least two are seated.

| URL | What |
|---|---|
| `/` | The game client |
| `/ws` | WebSocket endpoint (used by the client) |
| `/admin` | Admin page: live game state, scoreboard, Kick All |

## Tests

```sh
make test      # engine (66) + server (104) tests
```

## Security (room code + admin key)

Both are optional and controlled by environment variables. When unset,
anyone can join and the admin page is open — fine for local play.

```sh
TIENLEN_ROOM_CODE=secret TIENLEN_ADMIN_KEY=adminkey ./run
```

- `TIENLEN_ROOM_CODE` — players must enter this code once to join
  (the client prompts and remembers it for the session)
- `TIENLEN_ADMIN_KEY` — required for `/admin`, `/admin/status` and
  `/admin/kick-all` (pass as `?key=...` or `Authorization: Bearer ...`)

## Deploying to a webserver

### Option A: systemd (recommended)

```sh
# clone the repo to /opt/tienlen, then:
sudo cp tienlen.service /etc/systemd/system/
# edit the Environment= lines: room code, admin key, base path
sudo systemctl daemon-reload
sudo systemctl enable --now tienlen
```

Logs: `journalctl -u tienlen -f`. Restart after code changes:
`sudo systemctl restart tienlen`.

### Option B: daemon script

```sh
TIENLEN_ROOM_CODE=secret TIENLEN_ADMIN_KEY=adminkey \
  ./run_server.sh          # daemon with PID file, port 8090
```

`run_server.sh` passes standard `http_unix_daemon` options, e.g.
`--port=8090 --pidfile=/tmp/tienlen.pid --user=root`.

### Hosting under a subpath

If the server's root serves your main page, host the game under
`/tienlen/` instead:

```sh
TIENLEN_BASE_PATH=/tienlen ./run_server.sh
```

The client discovers its base path automatically from the URL it is
served from — no client configuration needed.

### nginx reverse proxy

The game needs two locations: one for HTTP, one for the WebSocket
(which requires upgrade headers and a long read timeout):

```nginx
location /tienlen {
    proxy_pass http://localhost:8090;
    include proxy_params;
}

location /tienlen/ws {
    proxy_pass http://localhost:8090;
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "upgrade";
    proxy_read_timeout 3600s;    # keep idle game connections alive
}
```

For HTTPS, terminate TLS at nginx (or use Caddy, which does it
automatically). The client switches to `wss://` on its own when served
over HTTPS.

## Environment variables

| Variable | Default | Purpose |
|---|---|---|
| `TIENLEN_ROOM_CODE` | *(unset = open)* | Code players must enter to join |
| `TIENLEN_ADMIN_KEY` | *(unset = open)* | Key for the admin endpoints |
| `TIENLEN_BASE_PATH` | `/` | Base path when hosting under a subpath |

## Ports

| Service | Port |
|---|---|
| TienLen (daemon mode) | 8090 |
| TienLen (local `./run`) | 8080 |

(8080 is also used by `ems_dish_db` — hence the different daemon port.)
