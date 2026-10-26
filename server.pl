% Single-game WebSocket server for TienLen.
%
% One implicit game at a time: the first 4 clients to connect become the
% players (in connection order). While no game is running everyone sees a
% lobby status; any client starts the game with a "new_game" message once
% at least two players are seated (2-4 players). Clients connecting while
% a game is running receive the current state as spectators and join the
% next game. The server owns the game state; clients only send moves and
% receive personalized views of the state.
%
% Protocol (JSON over WebSocket text frames):
%   client -> server: {"type":"move","action":"pass"}
%                  | {"type":"move","action":{"place":{"type":"single","card":{...}}}}
%                  | {"type":"move","action":{"place":{"type":"pair","cards":[...]}}}
%                  | {"type":"new_game"}
%   server -> client: the full game_state as JSON (see json_object declarations)
%
% Run with: swipl server.pl   (or ./run)

:- module(server, [start/0, handle_ws/3, handle_message/2, handle_move/2, handle_kick_all/0, apply_move/3, settle/2, place_term/2, game_is_over/1, extract_player_index/2, client_action_allowed/2, start_new_game/0, update_game/1, broadcast/1, player_view/3, send_json/2, seat_client/3, remove_client/1, players/1, game/1, handle_player_left/2, broadcast_event/2, bomb_beaten/2]).
:- use_module(library(http/http_server)).
:- use_module(library(http/websocket)).
:- use_module(library(http/http_files)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
% Older SWI-Prolog (CI's Ubuntu package) keeps these under library(http/);
% SWI 10 moved them to the top level and redirects the old paths.
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(tienlen).

% Running modes:
%   ./run.sh                 - foreground (local play)
%   ./run_server.sh          - daemon with PID file (webserver)
%   ./serve.sh               - quick background start
%   swipl -s server.pl ...   - load only (tests): nothing auto-starts
%
% When executed as a script (swipl server.pl), serve/0 runs as the main
% initialization: it registers the handlers and hands off to http_daemon,
% which parses standard options (--port, --pidfile, --user, ...).
:- initialization(serve, main).

% ---------------------------------------------------------------------------
% JSON shape of the game state (same declarations as before)
% ---------------------------------------------------------------------------

% rank is any (not (text;integer)): SWI 8.4's is_of_type/2 does not
% support type disjunctions, which made every card conversion throw
% type_error(json_term, card(...)) on the webserver. Atoms and integers
% pass through `any` unchanged, so the JSON output is identical.
:- json_object
    card(rank: any, suit: text).

:- json_object
    next_move(player, action: any).

:- json_object
    single(card: any) + [type=single].

:- json_object
    pair(cards: list) + [type=pair].

:- json_object
    sequence(cards: list) + [type=sequence].

:- json_object
    three_of_kind(cards: list) + [type=three_of_kind].

:- json_object
    four_of_kind(cards: list) + [type=four_of_kind].

:- json_object
    double_sequence(cards: list) + [type=double_sequence].

% hands      playerstates  cardsInPlay  discardedCards, attackerindex, scoreboard nextMove
:- json_object
    game_state(hands: list, player_states: list, cards_in_play: any, discarded_cards: list, attacker: any, scoreboard: list, next_move: any).

% ---------------------------------------------------------------------------
% Server state
% ---------------------------------------------------------------------------

% players(Sockets) in seat order; game(GS) while a game is running.
% session(Seat, Token): a player's persistent identity across reconnects
% (a page refresh gets a new socket; the token lets them reclaim their
% seat and hand instead of being seated as a new player).
% ws_token(WS, Token): the token each connection presented, so a
% spectator joining via the "Join game" button can be registered.
% player_name(WS, Name): the alias a client chose (unique in the room).
% last_scoreboard(SB): the most recent game's scoreboard, kept after the
% game is cleared so the admin view can still show it.
:- dynamic players/1, game/1, session/2, kicked_session/1, ws_token/2, player_name/2, last_scoreboard/1.

players([]).

% ---------------------------------------------------------------------------
% Security: room code (players) and admin key (admin actions).
%
% Set via environment variables before starting:
%   TIENLEN_ROOM_CODE  - players must enter this to join (default: none)
%   TIENLEN_ADMIN_KEY  - required for admin endpoints (default: none)
%   TIENLEN_PERSISTENT - "1" = persistent room mode: seats are owned by
%                        long-lived identity tokens, disconnects never
%                        free a seat, and the game state is saved to disk
%                        after every move (survives server restarts).
%                        For long-running games between trusted players.
%
% When a variable is unset/empty, that check is disabled (local play).
% ---------------------------------------------------------------------------

room_code(Code) :-
    getenv('TIENLEN_ROOM_CODE', Code), Code \== '', !.
room_code(none).

admin_key(Key) :-
    getenv('TIENLEN_ADMIN_KEY', Key), Key \== '', !.
admin_key(none).

% Persistent room mode (see above).
persistent_mode :-
    getenv('TIENLEN_PERSISTENT', V), V \== '', !.
persistent_mode :-
    getenv('TIENLEN_PERSISTENT', '1'), !.

% True when the request carries the correct room code (or no code is set).
authorized_room(_) :-
    room_code(C), C == none, !.
authorized_room(Request) :-
    room_code(Code),
    (   memberchk(search(Search), Request),
        memberchk(room=Code, Search)
    ->  true
    ;   memberchk(authorization(Header), Request),
        atomic_list_concat(['Room ', Code], Expected),
        Header == Expected
    ->  true
    ;   cookie_room(Request, Code)
    ).

% Room code from a cookie (set by the client after entering it once).
cookie_room(Request, Code) :-
    memberchk(cookie(Cookies), Request),
    memberchk(room=Code, Cookies).

% True when the request carries the correct admin key.
authorized_admin(_) :-
    admin_key(K), K == none, !.
authorized_admin(Request) :-
    admin_key(Key),
    (   memberchk(search(Search), Request),
        memberchk(key=Key, Search)
    ->  true
    ;   memberchk(authorization(Header), Request),
        atomic_list_concat(['Bearer ', Key], Expected),
        Header == Expected
    ).

start :-
    register_routes,
    load_room_state,
    % Listen on IPv6 any first (on macOS it accepts v4-mapped connections
    % too), falling back to plain IPv4. This keeps the server reachable
    % from same-Wi-Fi devices via link-local IPv6 even when a corporate
    % VPN hijacks IPv4 routing.
    (   catch(http_server([port(8080), bind('::')]), _, fail)
    ->  format(user_error, 'Listening on IPv6 (dual-stack)~n', [])
    ;   http_server([port(8080)]),
        format(user_error, 'Listening on IPv4~n', [])
    ),
    (   room_code(C), C == none
    ->  format(user_error, 'Room code: DISABLED (anyone can join)~n', [])
    ;   format(user_error, 'Room code: enabled~n', [])
    ),
    (   admin_key(K), K == none
    ->  format(user_error, 'Admin key: DISABLED~n', [])
    ;   format(user_error, 'Admin key: enabled~n', [])
    ),
    thread_get_message(never). % keep the main thread alive

% Daemon mode (like ems_dish_db): run as a background daemon managed by
% systemd. Reads standard http_unix_daemon options (--port, --pidfile,
% --user, --group, ...) from the command line.
% Base path for all routes. Set the TIENLEN_BASE_PATH environment
% variable (e.g. /tienlen) when hosting behind a reverse proxy under a
% subpath; defaults to root. The client discovers it automatically from
% the URL it was served from.
:- multifile http:location/3.
:- dynamic http:location/3.

base_path(Base) :-
    getenv('TIENLEN_BASE_PATH', Base), Base \== '', !.
base_path('/').

% Register the game's routes under the configured base path.
register_routes :-
    base_path(Base),
    (   Base == '/'
    ->  register_at(root)
    ;   atomic_list_concat(['', Base, '/'], PrefixAtom),
        assertz(http:location(tienlen, PrefixAtom, [])),
        register_at(tienlen)
    ).

% Register handlers at a location prefix. The location specs (e.g.
% root(admin)) are built with =.. because the prefix is only known at
% runtime when hosting under a subpath.
register_at(Prefix) :-
    register_one(Prefix, ws, ws_with_token, [spawn([])]),
    register_one(Prefix, check, handle_check_room, []),
    register_one(Prefix, admin, serve_admin_page, []),
    register_one(Prefix, 'admin/status', handle_admin_status, []),
    register_one(Prefix, 'admin/kick-all', handle_admin_kick, []),
    register_one(Prefix, 'admin/kick', handle_admin_kick_one, []),
    register_one(Prefix, 'admin/restart', handle_admin_restart, []),
    % File server: the handler closure is http_reply_from_files('.', [])
    Spec =.. [Prefix, '.'],
    Closure =.. [http_reply_from_files, '.', []],
    http_handler(Spec, Closure, [prefix]).

register_one(Prefix, Path, Handler, Options) :-
    Spec =.. [Prefix, Path],
    http_handler(Spec, Handler, Options).

serve :-
    register_routes,
    load_room_state,
    (   room_code(C), C == none
    ->  format(user_error, 'Room code: DISABLED (anyone can join)~n', [])
    ;   format(user_error, 'Room code: enabled~n', [])
    ),
    (   admin_key(K), K == none
    ->  format(user_error, 'Admin key: DISABLED~n', [])
    ;   format(user_error, 'Admin key: enabled~n', [])
    ),
    http_daemon.

% Extract the session token from the upgrade request, then hand off to
% the websocket handler with the token bound. The room code gates who
% may connect at all.
ws_with_token(Request) :-
    (   authorized_room(Request)
    ->  (   memberchk(search(Search), Request),
            (   memberchk(token=Token, Search) -> true ; Token = none ),
            (   memberchk(ident=Ident, Search) -> true ; Ident = none )
        ->  true
        ;   Token = none, Ident = none
        ),
        http_upgrade_to_websocket(handle_ws(Token, Ident), [], Request)
    ;   % Wrong or missing room code: refuse the upgrade.
        reply_json_dict(_{error:unauthorized}, [status(401)])
    ).

% Room-code probe for clients: a plain HTTP request can see a 401 (a
% WebSocket handshake failure is invisible to browsers), so the client
% can detect a stale stored code and re-prompt instead of retrying
% forever.
handle_check_room(Request) :-
    (   authorized_room(Request)
    ->  reply_json_dict(_{ok:true})
    ;   reply_json_dict(_{error:unauthorized}, [status(401)])
    ).

% Admin: live status (counts + full game state for the admin view).
handle_admin_status(Request) :-
    (   authorized_admin(Request)
    ->  players(Players),
        length(Players, N),
        spectators(Specs),
        length(Specs, S),
        % How many seats hold a dead socket (disconnected, in grace)?
        % The admin should see them as such, not as seated players.
        findall(Seat,
                (   nth0(Seat, Players, WS), \+ socket_alive(WS) ),
                DeadSeats),
        findall(Entry, roster_entry(Entry), Roster),
        (   game(GS)
        ->  GS = game_state(Hands, PlayerStates, CIP, _DC, _A, SB, next_move(P, Action)),
            maplist(length, Hands, HandCounts),
            json_convert:prolog_to_json(CIP, CIPJ, server),
            Game = _{running:true, turn:P, action:Action, cards_in_play:CIPJ,
                     hand_counts:HandCounts, player_states:PlayerStates, scoreboard:SB}
        ;   (   last_scoreboard(SB)
            ->  true
            ;   SB = []
            ),
            Game = _{running:false, scoreboard:SB}
        ),
        (   persistent_mode -> Persistent = true ; Persistent = false ),
        reply_json_dict(_{seated:N, spectators:S, game:Game, names:Roster,
                          disconnected:DeadSeats, persistent:Persistent})
    ;   reply_json_dict(_{error:unauthorized}, [status(401)])
    ).

% Admin: kick everyone. Accepts any method; returns a short confirmation.
% The kick runs in a separate thread: closing the WebSocket connections
% disrupts keep-alive HTTP connections, so the reply must complete first.
handle_admin_kick(Request) :-
    (   authorized_admin(Request)
    ->  format(user_error, 'ADMIN KICK requested~n', []),
        reply_json_dict(_{ok:true, kicked:all}),
        thread_create(admin_kick_thread, _, [detached(true)])
    ;   reply_json_dict(_{error:unauthorized}, [status(401)])
    ).

admin_kick_thread :-
    catch(with_mutex(tienlen_game, handle_kick_all),
          E, format(user_error, 'Admin kick error: ~q~n', [E])),
    !.
admin_kick_thread :-
    format(user_error, 'Admin kick thread finished~n', []).

% Admin: kick a single player by seat (or a spectator by index).
% POST /admin/kick?seat=N kicks seated player N; POST /admin/kick?spec=N
% kicks spectator N. The kick runs in a separate thread (see kick-all).
handle_admin_kick_one(Request) :-
    (   authorized_admin(Request)
    ->  (   memberchk(search(Search), Request),
            (   memberchk(seat=SeatAtom, Search), Kind = seat
            ;   memberchk(spec=SeatAtom, Search), Kind = spec
            )
        ->  (   catch(atom_number(SeatAtom, Seat), _, fail)
            ->  reply_json_dict(_{ok:true}),
                thread_create(admin_kick_one_thread(Kind, Seat), _, [detached(true)])
            ;   reply_json_dict(_{error:bad_seat}, [status(400)])
            )
        ;   reply_json_dict(_{error:missing_seat}, [status(400)])
        )
    ;   reply_json_dict(_{error:unauthorized}, [status(401)])
    ).

% Kick one seat: ban the token (so the kicked tab's auto-reconnect lands
% as a spectator, not back in the seat), then close the socket. The
% normal disconnect machinery (remove_client) frees the seat, runs the
% grace period if a game is running, and updates the lobby.
admin_kick_one_thread(Kind, Seat) :-
    catch(with_mutex(tienlen_game, handle_kick_one(Kind, Seat)),
          E, format(user_error, 'Admin kick-one error: ~q~n', [E])),
    !.
admin_kick_one_thread(_, _) :-
    format(user_error, 'Admin kick-one thread finished~n', []).

% Admin: restart the game (same as a player pressing Restart: a fresh
% deal for the currently seated players). POST /admin/restart.
handle_admin_restart(Request) :-
    (   authorized_admin(Request)
    ->  (   players(Players), length(Players, N), N >= 2
        ->  reply_json_dict(_{ok:true}),
            thread_create(admin_restart_thread, _, [detached(true)])
        ;   reply_json_dict(_{error:need_two_players}, [status(400)])
        )
    ;   reply_json_dict(_{error:unauthorized}, [status(401)])
    ).

% Run Goal with current_output redirected to a null stream. Threads
% spawned from HTTP workers inherit the client socket as current_output;
% once the reply is sent that socket closes, and the engine's writeln/1
% debug output would raise an existence_error and kill the thread.
with_null_output(Goal) :-
    open_null_stream(Null),
    setup_call_cleanup(set_output(Null), Goal, close(Null)).

admin_restart_thread :-
    catch(with_null_output(with_mutex(tienlen_game,
                         (   players(Players), length(Players, N), N >= 2
                         ->  start_new_game
                         ;   true
                         ))),
          E, format(user_error, 'Admin restart error: ~q~n', [E])),
    !.
admin_restart_thread :-
    format(user_error, 'Admin restart thread finished~n', []).

handle_kick_one(seat, Seat) :-
    players(Players),
    (   between(0, 3, Seat),
        nth0(Seat, Players, WS)
    ->  format(user_error, 'ADMIN KICK seat ~w~n', [Seat]),
        (   kicked_token(WS, Token), Token \== none
        ->  retractall(kicked_session(Token)), assertz(kicked_session(Token))
        ;   true
        ),
        % Persistent room: the kick frees the seat and clears the
        % ownership, so the next connection of that identity is a
        % spectator again (the admin's escape hatch for stuck seats).
        (   persistent_mode
        ->  retractall(seat_owner(Seat, _)),
            save_room_state
        ;   true
        ),
        catch(ws_close(WS, 1000, 'kicked'), _, true)
    ;   format(user_error, 'ADMIN KICK: no such seat ~w~n', [Seat])
    ).
handle_kick_one(spec, Seat) :-
    spectators(Specs),
    (   nth0(Seat, Specs, WS)
    ->  format(user_error, 'ADMIN KICK spectator ~w~n', [Seat]),
        (   kicked_token(WS, Token), Token \== none
        ->  retractall(kicked_session(Token)), assertz(kicked_session(Token))
        ;   true
        ),
        catch(ws_close(WS, 1000, 'kicked'), _, true)
    ;   format(user_error, 'ADMIN KICK: no such spectator ~w~n', [Seat])
    ).

% Admin page: a static page (admin.html) served from the repo root; it
% polls /admin/status for the live game state and scoreboard.
serve_admin_page(Request) :-
    (   authorized_admin(Request)
    ->  http_reply_file('admin.html', [], Request)
    ;   % Ask for the key via basic-style prompt, or show a hint.
        reply_html_page(title('TienLen admin'),
                        [ h1('TienLen admin')
                        , p('Unauthorized. Append ?key=YOUR_ADMIN_KEY to the URL.')
                        ])
    ).


% ---------------------------------------------------------------------------
% Connection lifecycle
% ---------------------------------------------------------------------------

% Seat once per connection, then process messages until the connection
% closes. The loop must not re-seat: a kicked or dying socket's error path
% would otherwise re-add the dead socket to the lobby.
handle_ws(Token, Ident, WS) :-
    with_mutex(tienlen_game, seat_client(WS, Token, Ident)),
    handle_ws_loop(WS).

% A dropped connection (tab closed, network loss) surfaces as an
% exception from ws_receive — not a close opcode — so both the normal
% close and the error path must remove the client, or the seat leaks
% and the lobby count never drops.
handle_ws_loop(WS) :-
    catch(ws_receive(WS, Msg),
          E,
          (   format(user_error, 'Connection error: ~q~n', [E]),
              Msg = websocket{opcode:close, data:end_of_file}
          )),
    (   Msg.opcode == close
    ->  with_mutex(tienlen_game, remove_client(WS))
    ;   catch(with_mutex(tienlen_game, handle_message(WS, Msg.data)),
              E, report_error(WS, E)),
        handle_ws_loop(WS)
    ).

% First 4 connections become players (in connection order); later ones are
% spectators. A client presenting a session token that matches a departed
% player reclaims that seat (page refresh keeps your hand). While no game
% is running, everyone sees the lobby status; a game is started with a
% "new_game" message once at least 2 are seated.
seat_client(WS, Token, Ident) :-
    % Remember the tokens for this connection: the per-tab session token
    % (seat reclaim on refresh) and, in persistent mode, the long-lived
    % identity token (seat ownership).
    retractall(ws_token(WS, _)),
    (   Token \== none
    ->  assertz(ws_token(WS, Token))
    ;   true
    ),
    (   Ident \== none
    ->  assertz(ws_ident(WS, Ident))
    ;   true
    ),
    % Fresh connection: stamp the heartbeat clock so the watchdog does
    % not drop them before their first message, and make sure the
    % watchdog is running (it must start after the daemon fork, so it
    % is created lazily on the first connection).
    catch((get_time(T), retractall(last_seen(WS, _)), assertz(last_seen(WS, T))), _, true),
    catch(start_watchdog, _, true),
    players(List),
    (   memberchk(WS, List) -> true    % already seated (shouldn't happen)
    ;   persistent_mode,
        ws_ident(WS, Ident), Ident \== none,
        seat_owner(Seat, Ident)
    ->  % Persistent room: the identity token owns a seat. Take it back
        % regardless of the old socket's state - the seat is theirs.
        replace_at(Seat, List, WS, NewList),
        retract(players(List)), assertz(players(NewList)),
        format(user_error, 'Owner ~w reclaimed seat ~w~n', [Ident, Seat]),
        (   game(GS)
        ->  player_view(GS, Seat, View),
            send_json(WS, View),
            send_names(WS)
        ;   broadcast_lobby
        )
    ;   Token \== none, kicked_session(Token)
    ->  % Kicked by the admin: watch as a spectator until the next game.
        format(user_error, 'Kicked client reconnects as spectator~n', []),
        spectator_join(WS)
    ;   Token \== none,
        session(Seat, Token),
        nth0(Seat, List, OldSocket),
        \+ socket_alive(OldSocket)        % the old socket must be gone
    ->  % Reclaim the seat: replace the dead socket, keep the session.
        replace_at(Seat, List, WS, NewList),
        retract(players(List)), assertz(players(NewList)),
        format(user_error, 'Player ~w reconnected (seat reclaimed)~n', [Seat]),
        broadcast_event(grace_ended, Seat),   % clear any countdown
        (   game(GS)
        ->  player_view(GS, Seat, View),
            send_json(WS, View),
            send_names(WS)
        ;   broadcast_lobby
        )
    ;   % Everyone else starts as a spectator: joining the game is an
        % explicit choice (the "Join game" button), so people who just
        % want to watch don't occupy a seat.
        format(user_error, 'Spectator joined~n', []),
        spectator_join(WS)
    ).

% Seat a fresh connection as a spectator: they see the game (hands
% hidden) or the lobby, and are registered for promotion when a player
% departs.
spectator_join(WS) :-
    (   game(GS)
    ->  player_view(GS, -1, View),   % seat -1: no hand is yours
        send_json(WS, View)
    ;   players(Players),
        length(Players, Seated),
        Needed is max(2 - Seated, 0),
        send_json(WS, json([type=waiting, seated=Seated, needed=Needed, spectator=true]))
    ),
    spectators(Specs),
    (   memberchk(WS, Specs) -> true
    ;   retract(spectators(Specs)), assertz(spectators([WS | Specs]))
    ),
    send_names(WS).

% ---------------------------------------------------------------------------
% Persistent room mode: seats are owned by identity tokens, not sockets.
% A disconnect never frees the seat; the owner reconnects with the same
% token and picks up exactly where they left off. The game state is
% saved to disk after every change and reloaded on startup.
% ---------------------------------------------------------------------------

% seat_owner(Seat, Token): this seat belongs to this identity token.
:- dynamic seat_owner/2.

% ws_ident(WS, Ident): the long-lived identity token this connection
% presented (persistent mode only).
:- dynamic ws_ident/2.

% Save the room state to disk (persistent mode only). Called after every
% game state change; the file is read back on startup. Must always
% succeed: a failure here would kill the message handler thread (and
% with it the connection). Sockets are NOT saved (meaningless after a
% restart): instead the owner identity token and alias of each seat are
% stored, and the seats are rebuilt as placeholders on load.
save_room_state :-
    \+ persistent_mode, !.
save_room_state :-
    catch((players(Players),
           findall(Owner,
                   (   nth0(Seat, Players, _),
                       (   seat_owner(Seat, Owner) -> true ; Owner = none )
                   ),
                   Owners),
           findall(Name,
                   (   nth0(Seat, Players, WS),
                       (   player_name(WS, Name) -> true ; Name = null )
                   ),
                   Names),
           (   game(GS) -> true ; GS = none ),
           open('room_state.pl', write, Stream),
           write_term(Stream, room_state(Owners, Names, GS), [quoted(true)]),
           write(Stream, '.\n'),
           close(Stream)),
          E, format(user_error, 'Room state save failed: ~q~n', [E])).

% Load the room state from disk on startup (persistent mode only). The
% seats are rebuilt as placeholder atoms (dead sockets): the owners
% reclaim them on reconnect via their identity tokens.
load_room_state :-
    \+ persistent_mode, !.
load_room_state :-
    (   catch((open('room_state.pl', read, Stream),
               read_term(Stream, room_state(Owners, Names, GS), []),
               close(Stream)),
              _, fail)
    ->  length(Owners, N),
        findall(WS,
                (   between(1, N, I),
                    atomic_list_concat(['persist_seat_', I], WS)
                ),
                Players),
        retractall(players(_)), assertz(players(Players)),
        forall((nth0(Seat, Owners, Owner), Owner \== none),
               assertz(seat_owner(Seat, Owner))),
        forall((nth0(Seat, Players, WS), nth0(Seat, Names, Name), Name \== null),
               assertz(player_name(WS, Name))),
        (   GS == none
        ->  true    % no game was running: nothing to restore
        ;   retractall(game(_)), assertz(game(GS))
        ),
        format(user_error, 'Persistent room state loaded: ~w seat(s)~n', [N])
    ;   format(user_error, 'No persistent room state found (fresh room)~n', [])
    ).

% Lobby status: how many players are seated and how many more are needed.
% Spectators get a spectator=true flag so the client can offer the
% "Join game" button (the manual escape from the kick-all ban).
broadcast_lobby :-
    players(Players),
    length(Players, N),
    Needed is max(2 - N, 0),
    forall(member(WS, Players),
           send_json(WS, json([type=waiting, seated=N, needed=Needed]))),
    spectators(Specs),
    forall(member(WS, Specs),
           send_json(WS, json([type=waiting, seated=N, needed=Needed, spectator=true]))),
    broadcast_names.

% Sockets of players who left mid-game (pruned when a new game starts).
:- dynamic left_seats/1.
left_seats([]).

% Last time each connected socket proved it is alive (any message, or
% the client's heartbeat ping). The watchdog drops sockets that stay
% silent past the window: a phone in standby freezes the page while
% the OS keeps the TCP connection half-open, so no close frame ever
% arrives and the game would otherwise wait forever for a zombie seat.
:- dynamic last_seen/2.   % last_seen(WS, Time)

% Drop clients whose heartbeat has been silent past the window (the
% client beats every 20s; 75s means three missed beats). Removal runs
% under the game mutex; the socket close happens in its own thread
% because the close handshake can block on a half-open connection.
% Started lazily on the first connection: daemon mode forks inside
% http_daemon/0, which silently discards threads created before it.
start_watchdog :-
    (   watchdog_running
    ->  true
    ;   asserta(watchdog_running),
        thread_create(watchdog_loop, _, [detached(true)])
    ).

:- dynamic watchdog_running/0.

watchdog_loop :-
    sleep(30),
    catch(drop_silent_clients, E,
          format(user_error, 'Watchdog error: ~q~n', [E])),
    watchdog_loop.

drop_silent_clients :-
    (   persistent_mode
    ->  true    % persistent rooms: silent sockets are fine (owners away)
    ;   with_mutex(tienlen_game, silent_sockets(Silent)),
        forall(member(WS, Silent),
               thread_create((catch(ws_close(WS, 1001, 'heartbeat timeout'), _, true) -> true ; true),
                             _, [detached(true)]))
    ).

silent_sockets(Silent) :-
    get_time(Now),
    findall(WS,
            (   last_seen(WS, T),
                Now - T > 75,
                connected_socket(WS)
            ),
            Silent),
    (   Silent == []
    ->  true
    ;   format(user_error, 'Watchdog dropping ~w silent socket(s)~n', [Silent]),
        forall(member(WS, Silent), remove_client(WS))
    ).

connected_socket(WS) :-
    players(Players), member(WS, Players).
connected_socket(WS) :-
    spectators(Specs), member(WS, Specs).

% Spectators: connections beyond the 4 seats. They watch the game and are
% promoted into a seat when a player departs.
:- dynamic spectators/1.
spectators([]).

remove_client(WS) :-
    retractall(dead_socket(WS)),          % final removal: forget the marker
    retractall(ws_token(WS, _)),
    retractall(ws_ident(WS, _)),
    retractall(player_name(WS, _)),
    retractall(last_seen(WS, _)),          % no heartbeat to track anymore
    retractall(seat_declined(_, WS)),     % their seat declinations are moot
    players(List),
    (   select(WS, List, Rest)
    ->  nth0(Seat, List, WS),
        (   persistent_mode
        ->  % Persistent room: the seat is owned by an identity token,
            % not the socket. A disconnect never frees it - the owner
            % reconnects and picks up where they left off. The socket
            % stays as a placeholder so seat indices stay stable.
            format(user_error, 'Owner of seat ~w disconnected (seat kept)~n', [Seat]),
            (   game(GS), \+ game_is_over(GS), seat_in_game(Seat, GS)
            ->  broadcast_event(grace_started, Seat)   % show "away" state
            ;   true
            )
        ;   game(GS), \+ game_is_over(GS), seat_in_game(Seat, GS)
        ->  % Mid-game departure: give the player a grace period to
            % reconnect (page refresh) before treating them as gone. The
            % socket stays as a seat placeholder meanwhile. Everyone sees
            % a countdown so they know why the game is paused.
            format(user_error, 'Player ~w disconnected mid-game (grace period)~n', [Seat]),
            broadcast_event(grace_started, Seat),
            thread_create(grace_timeout(Seat), _, [detached(true)])
        ;   retract(players(List)), assertz(players(Rest)),
            format(user_error, 'Client disconnected, ~w player(s) left~n', [Rest]),
            (   game_is_over_running, length(Rest, N), N < 2
            ->  % Game over and too few players remain to start a new one:
                % return to the lobby instead of showing a stale
                % game-over screen with nobody to play with.
                format(user_error, 'Game over, ~w player(s) left: back to lobby~n', [N]),
                retractall(game(_)),
                % The seats compacted: any recorded departed seats are
                % stale (their indices shifted), and a stale entry would
                % prune a live player at the next game start.
                retractall(left_seats(_)), assertz(left_seats([])),
                broadcast_lobby
            ;   game(_)
            ->  true                        % mid-game views update via events
            ;   broadcast_lobby
            )
            % No seat offer here: the list just compacted, so the departed
            % player's seat index is gone. The lobby broadcast above already
            % shows every spectator the "Join as player" button, and a
            % mid-game departure offers its seat via the grace-timeout path
            % (where the placeholder keeps the indices stable).
        )
    ;   % A spectator disconnected: drop them from the promotion queue.
        spectators(Specs),
        (   select(WS, Specs, RemainingSpecs)
        ->  retract(spectators(Specs)), assertz(spectators(RemainingSpecs))
        ;   true
        )
    ).

seat_in_game(Seat, game_state(Hands, _, _, _, _, _, _)) :-
    length(Hands, N),
    Seat < N.

% True when a game is running and already over.
game_is_over_running :-
    game(GS),
    game_is_over(GS).

% Grace period for mid-game disconnects: wait, then if the player has not
% reconnected (their seat still holds a dead socket), treat them as gone.
% A seat that keeps flapping (zombie tab auto-reconnecting without a human)
% is released after a few cycles so the game does not stall forever.
:- dynamic grace_flaps/2.   % grace_flaps(Seat, Count)

grace_timeout(Seat) :-
    (   persistent_mode
    ->  true    % persistent rooms: the seat is owned, never released
    ;   sleep(10),                       % reconnect window (page refresh)
        with_mutex(tienlen_game,
               (   players(List),
                   (   nth0(Seat, List, Sock), \+ socket_alive(Sock),
                       game(GS), \+ game_is_over(GS), seat_in_game(Seat, GS)
                   ->  format(user_error, 'Player ~w left mid-game (grace expired)~n', [Seat]),
                       handle_player_left(Seat, GS),
                       promote_spectator(Seat),
                       broadcast_event(grace_ended, Seat),
                       retractall(grace_flaps(Seat, _))
                   ;   (   nth0(Seat, List, Sock2), socket_alive(Sock2)
                       ->  % reconnected: count the flap; too many = zombie
                           (   retract(grace_flaps(Seat, C))
                           ->  C1 is C + 1
                           ;   C1 = 1
                           ),
                           assertz(grace_flaps(Seat, C1)),
                           (   C1 >= 3
                           ->  format(user_error, 'Player ~w seat released (repeated disconnects)~n', [Seat]),
                               (   game(GS2), \+ game_is_over(GS2), seat_in_game(Seat, GS2)
                               ->  handle_player_left(Seat, GS2),
                                   promote_spectator(Seat),
                                   broadcast_event(grace_ended, Seat)
                               ;   true
                               ),
                               retractall(grace_flaps(Seat, _))
                           ;   true
                           )
                       ;   % Dead socket with no live game to rejoin (the
                           % game ended meanwhile, or the seat is outside
                           % the game): release the seat now instead of
                           % waiting for the network to notice the dead
                           % connection. Sweep every such zombie at once -
                           % removing one-by-one would shift seat indices
                           % under the other pending grace timers.
                           reap_dead_players
                       )
                   )
               )
        )
    ).

% Remove every seated socket that is dead and has no live game to rejoin
% (no game, the game is over, or the seat is not part of the running
% game). A phone in standby leaves a half-open TCP connection that the
% network may not notice for minutes; without this sweep the lobby (and
% the admin view) keeps counting zombies as seated players.
reap_dead_players :-
    (   persistent_mode
    ->  true    % persistent rooms: dead sockets are fine, owners return
    ;   players(Players),
        findall(WS,
                (   nth0(Seat, Players, WS),
                    \+ socket_alive(WS),
                    \+ ( game(GS), \+ game_is_over(GS), seat_in_game(Seat, GS) )
                ),
                Dead),
        (   Dead == []
        ->  true
        ;   format(user_error, 'Reaping ~w dead seat(s)~n', [Dead]),
            forall(member(WS, Dead), remove_client(WS))
        )
    ).

% Offer the freed seat to all waiting spectators: first to accept wins.
% (A single targeted offer can be lost to a stale socket; broadcasting
% makes the flow robust.) Spectators who already declined this seat are
% skipped: they keep watching but don't get re-prompted.
promote_spectator(Seat) :-
    spectators(Specs),
    exclude([WS]>>(seat_declined(Seat, WS)), Specs, Eligible),
    (   Eligible == []
    ->  true
    ;   format(user_error, 'Offering seat ~w to ~w spectator(s)~n', [Seat, Eligible]),
        forall(member(WS, Eligible),
               send_json(WS, json([type=event, event=seat_offer, seat=Seat])))
    ).

% A spectator explicitly requests an open seat (the "Join" button).
% Only legal if the seat is actually free (its socket departed).
request_seat(WS, Seat) :-
    spectators(Specs),
    (   memberchk(WS, Specs)
    ->  players(Players),
        (   nth0(Seat, Players, Departed),
            \+ socket_alive(Departed)
        ->  accept_seat(WS, Seat)
        ;   send_json(WS, json([type=error, message=seat_taken]))
        )
    ;   send_json(WS, json([type=error, message=not_a_spectator]))
    ).

% Best-effort liveness probe: check the websocket's underlying streams.
% A closed/dead stream reports status(closed); a healthy one does not.
socket_alive(WS) :-
    catch((stream_pair(WS, In, Out),
           (   var(Out)
           ->  false
           ;   catch(ws_property(Out, status, closed), _, false)
           ->  false
           ;   true
           ),
           (   var(In)
           ->  false
           ;   catch(ws_property(In, status, closed), _, false)
           ->  false
           ;   true
           )),
          _, fail).

% The spectator accepted: seat them. First acceptor wins; later accepts
% find the seat already taken by a live socket and are rejected.
accept_seat(WS, Seat) :-
    spectators(Specs),
    (   memberchk(WS, Specs)
    ->  players(Players),
        (   nth0(Seat, Players, Current)
        ->  (   socket_alive(Current)
            ->  send_json(WS, json([type=error, message=seat_taken]))
            ;   select(WS, Specs, Remaining),
                retract(spectators(Specs)), assertz(spectators(Remaining)),
                replace_at(Seat, Players, WS, NewPlayers),
                retract(players(Players)), assertz(players(NewPlayers)),
                % The seat is occupied again: it is no longer departed, so
                % the next game must not prune the new player.
                forget_left_seat(Seat),
                retractall(seat_declined(Seat, _)),
                % A deliberate seat-taking: lift the kick ban and register
                % the session so a page refresh reclaims the seat.
                register_session(WS, Seat),
                format(user_error, 'Spectator accepted seat ~w~n', [Seat]),
                (   game(GS)
                ->  player_view(GS, Seat, View),
                    send_json(WS, View),
                    broadcast_names
                ;   broadcast_lobby
                ),
                broadcast_event(promoted, Seat),
                send_json(WS, json([type=event, event=you_are_seated, seat=Seat]))
            )
        ;   % The seat index is gone (lobby compacted after a departure):
            % the seat it referred to no longer exists.
            send_json(WS, json([type=error, message=seat_taken]))
        )
    ;   true    % not a spectator anymore (e.g. already promoted)
    ).

% Spectators who declined a specific seat offer: they stay spectators
% (they keep watching and can still join later), but are not offered
% the same seat again.
:- dynamic seat_declined/2.   % seat_declined(Seat, WS)

% The spectator declined: keep them as a spectator, but don't offer them
% this seat again; offer it to the others.
decline_seat(WS, Seat) :-
    spectators(Specs),
    (   memberchk(WS, Specs)
    ->  (   seat_declined(Seat, WS)
        ->  true
        ;   assertz(seat_declined(Seat, WS))
        ),
        format(user_error, 'Spectator declined seat ~w~n', [Seat]),
        promote_spectator(Seat)         % offer to the others
    ;   true
    ).

% A player left mid-game: empty their hand and record them as out (with
% the next placement, so the engine's placement bookkeeping stays
% consistent). If it was their turn, advance so the game does not stall,
% and end the game if too few players remain.
handle_player_left(Seat, GS) :-
    left_seats(Left),
    (   memberchk(Seat, Left)
    ->  true                            % already handled
    ;   retractall(left_seats(_)), assertz(left_seats([Seat | Left])),
        GS = game_state(Hands, PlayerStates, CIP, DC, Attacker, SB, next_move(Whose, Action)),
        replace_at(Seat, Hands, [], NewHands),
        GS1 = game_state(NewHands, PlayerStates, CIP, DC, Attacker, SB, next_move(Whose, Action)),
        (   Whose == Seat
        ->  (   CIP == none
            ->  % The leaver had the lead on an empty table: hand the
                % lead to the next player who still has cards (the engine's
                % pass machinery cannot handle this case - it would keep
                % the turn on the departed player).
                next_player(NewHands, PlayerStates, Seat, NextSeat),
                GS4 = game_state(NewHands, PlayerStates, none, DC, NextSeat, SB,
                                 next_move(NextSeat, make_move))
            ;   % Cards in play: advance via the pass machinery so the
                % next player gets make_move or a forced pass.
                pass_move(GS1, GS2),
                interpret_tienlen(GS2, GS3),
                settle(GS3, GS4)
            )
        ;   GS4 = GS1
        ),
        % Mark the leaver out with the next placement AFTER advancing, so
        % the pass machinery does not overwrite it.
        GS4 = game_state(Hands4, States4, CIP4, DC4, A4, SB4, NM4),
        next_placement(States4, Place),
        replace_at(Seat, States4, Place, NewStates4),
        GS5 = game_state(Hands4, NewStates4, CIP4, DC4, A4, SB4, NM4),
        force_game_over_if_needed(GS5, NewGS),
        broadcast_event(left, Seat),
        update_game(NewGS)
    ).

% The game must end when at most one player still holds cards (the engine
% only checks this when placing, which the remaining players may never do).
force_game_over_if_needed(GS0, GS0) :-
    GS0 = game_state(_, _, _, _, _, _, next_move(_, game_over)), !.
force_game_over_if_needed(GS0, GS) :-
    GS0 = game_state(Hands, States, CIP, DC, A, SB, _),
    include(\=([]), Hands, NonEmpty),
    length(NonEmpty, N),
    (   N =< 1
    ->  next_placement(States, LastPlace),
        maplist([S, M] >> (S == in_play -> M = LastPlace ; M = S), States, FinalStates),
        GS = game_state(Hands, FinalStates, CIP, DC, A, SB, next_move(0, game_over))
    ;   GS = GS0
    ).

% ---------------------------------------------------------------------------
% Small list helpers (0-based)
% ---------------------------------------------------------------------------

replace_at(0, [_ | T], New, [New | T]) :- !.
replace_at(N, [H | T], New, [H | R]) :-
    N1 is N - 1,
    replace_at(N1, T, New, R).

remove_at(0, [_ | T], T) :- !.
remove_at(N, [H | T], [H | R]) :-
    N1 is N - 1,
    remove_at(N1, T, R).

is_placement(first).
is_placement(second).
is_placement(third).
is_placement(fourth).

% Normalize a name: trim leading/trailing whitespace, collapse inner
% runs to single spaces, cap at 12 characters.
normalize_name(Codes, Normalized) :-
    maplist([C, S] >> (code_type(C, space) -> S = 32 ; S = C), Codes, Spaced0),
    strip_leading_spaces(Spaced0, Spaced),
    collapse_spaces(Spaced, Collapsed0),
    strip_trailing(Collapsed0, Collapsed),
    (   Collapsed == []
    ->  !, fail                       % empty (or all-whitespace) name
    ;   length(Collapsed, Len), Len > 12
    ->  length(Capped, 12), append(Capped, _, Collapsed), !, Normalized = Capped
    ;   !, Normalized = Collapsed
    ).

% Collapse runs of spaces to a single space.
collapse_spaces([], []).
collapse_spaces([32 | T], [32 | R]) :-
    !,
    strip_leading_spaces(T, T1),
    collapse_spaces(T1, R).
collapse_spaces([C | T], [C | R]) :-
    collapse_spaces(T, R).

strip_leading_spaces([32 | T], R) :- !, strip_leading_spaces(T, R).
strip_leading_spaces(T, T).

strip_trailing([32], []) :- !.
strip_trailing([C | T], [C | R]) :- !, strip_trailing(T, R).
strip_trailing([], []).

% The next placement in sequence given the states so far.
next_placement(States, Place) :-
    include(is_placement, States, Placed),
    length(Placed, Count),
    Next is min(Count + 1, 4),
    place_for_count(Next, Place).

place_for_count(1, first).
place_for_count(2, second).
place_for_count(3, third).
place_for_count(4, fourth).

report_error(WS, E) :-
    format(user_error, 'Error handling message: ~q~n', [E]),
    send_json(WS, json([type=error, message=unknown_move])).

% ---------------------------------------------------------------------------
% Message dispatch
% ---------------------------------------------------------------------------

handle_message(WS, Data) :-
    atom_json_dict(Data, Dict, [value_string_as(atom)]),
    (   Dict.type == move
    ->  handle_move(WS, Dict)
    ;   Dict.type == new_game
    ->  handle_new_game(WS)
    ;   Dict.type == accept_seat
    ->  accept_seat(WS, Dict.seat)
    ;   Dict.type == decline_seat
    ->  decline_seat(WS, Dict.seat)
    ;   Dict.type == request_seat
    ->  request_seat(WS, Dict.seat)
    ;   Dict.type == join
    ->  handle_join(WS)
    ;   Dict.type == set_name
    ->  handle_set_name(WS, Dict.name)
    ;   Dict.type == start_over
    ->  handle_start_over(WS)
    ;   Dict.type == ping
    ->  send_json(WS, json([type=pong]))   % heartbeat reply
    ;   send_json(WS, json([type=error, message=unknown_message]))
    ),
    % Any message proves the client is alive: stamp it. The watchdog
    % drops sockets that stay silent past the heartbeat window.
    catch((get_time(T), retractall(last_seen(WS, _)), assertz(last_seen(WS, T))), _, true).

% ---------------------------------------------------------------------------
% Player aliases
% ---------------------------------------------------------------------------

% A client sets (or changes) their alias. Names are capped at 12
% characters and must be unique among the currently connected clients
% (players and spectators). A duplicate is auto-suffixed ("Dan" taken ->
% "Dan2") instead of rejected, so nobody has to re-prompt. On success
% everyone is told the new roster.
handle_set_name(WS, Name0) :-
    (   atom(Name0)
    ->  atom_codes(Name0, Codes)
    ;   string(Name0)
    ->  string_codes(Name0, Codes)
    ;   Codes = []
    ),
    (   normalize_name(Codes, Trimmed)
    ->  (   Trimmed == []
        ->  send_json(WS, json([type=error, message=name_empty]))
        ;   atom_codes(Base, Trimmed),
            unique_name(Base, WS, Name),
            retractall(player_name(WS, _)),
            assertz(player_name(WS, Name)),
            format(user_error, 'Client set name: ~w~n', [Name]),
            broadcast_names
        )
    ;   send_json(WS, json([type=error, message=name_empty]))
    ).

% The name to assign: Base itself if free, else Base with the lowest
% numeric suffix that is free (truncating the base so the total stays
% within 12 characters).
unique_name(Base, WS, Name) :-
    \+ name_taken(Base, WS),
    !,
    Name = Base.
unique_name(Base, WS, Name) :-
    between(2, 99, N),
    number_codes(N, NSuffixCodes),
    atom_codes(NSuffix, NSuffixCodes),
    length(NSuffixCodes, SuffixLen),
    BaseLen is 12 - SuffixLen,
    (   atom_length(Base, BaseLen0), BaseLen0 =< BaseLen
    ->  BaseTrunc = Base
    ;   sub_atom(Base, 0, BaseLen, _, BaseTrunc)
    ),
    atom_concat(BaseTrunc, NSuffix, Candidate),
    \+ name_taken(Candidate, WS),
    !,
    Name = Candidate.
% Everything from 2 to 99 is taken (needs 8+ same-base names): give up
% and append the full number even if it exceeds 12 characters.
unique_name(Base, _, Name) :-
    atom_concat(Base, '99', Name).

% True when another connected client already uses Name.
name_taken(Name, WS) :-
    connected_client(Other),
    Other \== WS,
    player_name(Other, Name).

connected_client(WS) :-
    players(Players), member(WS, Players).
connected_client(WS) :-
    spectators(Specs), member(WS, Specs).

% Tell everyone the current aliases: a list of name entries with the
% seat (or -1 for spectators), so clients can label hands and toasts.
broadcast_names :-
    players(Players),
    spectators(Specs),
    append(Players, Specs, All),
    forall(member(WS, All), send_names(WS)).

% The current alias roster, sent to one client.
send_names(WS) :-
    findall(Entry, roster_entry(Entry), Entries),
    send_json(WS, json([type=names, names=Entries])).

% One roster entry: seat and alias for every connected client.
roster_entry(Entry) :-
    players(Players),
    nth0(Seat, Players, PWS),
    client_name(PWS, Name),
    Entry = json([seat=Seat, name=Name]).
roster_entry(Entry) :-
    spectators(Specs),
    member(SWS, Specs),
    client_name(SWS, Name),
    Entry = json([seat= -1, name=Name]).

client_name(WS, Name) :-
    player_name(WS, Name),
    !.
client_name(_, null).

% A spectator asks to join the lobby as a player (the "Join game"
% button). Unlike auto-seating, this clears the client's kick ban: a
% deliberate click proves a human is present, which is exactly what the
% ban was meant to filter out (zombie tabs never click). This is the
% escape hatch from the kick-all deadlock: after a kick everyone
% reconnects as a banned spectator, and without this the lobby could
% never refill.
handle_join(WS) :-
    spectators(Specs),
    (   memberchk(WS, Specs)
    ->  players(Players),
        (   departed_seat(Players, Seat)
        ->  % A departed player's seat is open: take it instead of
            % appending (the joiner is dealt in there at the next game).
            accept_seat(WS, Seat)
        ;   length(Players, N),
            (   N < 4
            ->  select(WS, Specs, Remaining),
                retract(spectators(Specs)), assertz(spectators(Remaining)),
                append(Players, [WS], NewPlayers),
                retract(players(Players)), assertz(players(NewPlayers)),
                % A human clicked: lift the kick ban and register the
                % session so a page refresh reclaims this seat.
                register_session(WS, N),
                % Persistent room: the identity token now owns this seat
                % (a disconnect will not free it).
                (   persistent_mode,
                    ws_ident(WS, Ident), Ident \== none
                ->  retractall(seat_owner(_, Ident)),
                    assertz(seat_owner(N, Ident)),
                    save_room_state
                ;   true
                ),
                format(user_error, 'Spectator joined as player ~w~n', [N]),
                % Seat confirmation first, so the client knows its seat
                % when the "joined" toast arrives (it labels the joiner).
                send_json(WS, json([type=event, event=you_are_seated, seat=N])),
                (   game(GS)
                ->  % A game is running: the joiner has no hand yet (dealt
                    % in at the next game), so send them the current view.
                    % The lobby broadcast must NOT fire here: it would
                    % reset every client's game view back to the lobby
                    % screen mid-game.
                    player_view(GS, N, View),
                    send_json(WS, View),
                    broadcast_names
                ;   broadcast_lobby        % includes broadcast_names
                ),
                broadcast_event(joined, N)
            ;   send_json(WS, json([type=error, message=lobby_full]))
            )
        )
    ;   send_json(WS, json([type=error, message=not_a_spectator]))
    ).

% A seat left open by a departed player (recorded in left_seats, socket
% still gone): free for a joining spectator to take.
departed_seat(Players, Seat) :-
    left_seats(Left),
    member(Seat, Left),
    nth0(Seat, Players, Departed),
    \+ socket_alive(Departed).

% A deliberate seat-taking (join button or accepted offer): lift the
% kick ban and register the session so a page refresh reclaims the seat.
register_session(WS, Seat) :-
    (   ws_token(WS, Token), Token \== none
    ->  retractall(kicked_session(Token)),
        retractall(session(_, Token)),
        assertz(session(Seat, Token))
    ;   true
    ).

% The seat is occupied again: forget it as departed, so the next game
% does not prune the newly seated player.
forget_left_seat(Seat) :-
    left_seats(Left),
    (   select(Seat, Left, Rest)
    ->  retractall(left_seats(_)), assertz(left_seats(Rest))
    ;   true
    ).

% Close every connection and reset the lobby so clients can reconnect and
% reseat in a new order. Clients auto-reconnect and land back in the lobby.
% Clear the lobby BEFORE closing sockets: ws_close's close handshake can
% block, and reconnecting clients must not find the dead sockets seated.
% Kicked session tokens are banned from seating: their tabs auto-reconnect
% and would instantly re-take all the seats otherwise. They watch as
% spectators until the next game starts fresh.
handle_kick_all :-
    players(Players),
    spectators(Specs),
    % Collect the kicked tokens so their reconnects don't re-seat.
    findall(Token, (member(WS, Players), kicked_token(WS, Token)), PlayerTokens),
    findall(Token, (member(WS, Specs), kicked_token(WS, Token)), SpecTokens),
    append(PlayerTokens, SpecTokens, AllTokens),
    forall(member(T, AllTokens),
           (   retractall(kicked_session(T)), assertz(kicked_session(T)) )),
    retractall(players(_)), assertz(players([])),
    retractall(spectators(_)), assertz(spectators([])),
    retractall(game(_)),
    retractall(left_seats(_)), assertz(left_seats([])),
    retractall(session(_, _)),
    retractall(seat_owner(_, _)),
    retractall(previous_winner(_, _)), asserta(previous_winner(none, none)),
    retractall(dead_socket(_)),
    retractall(ws_token(_, _)),
    retractall(player_name(_, _)),
    retractall(seat_declined(_, _)),
    retractall(last_scoreboard(_)),
    format(user_error, 'Kicking all players (~w)~n', [Players]),
    forall(member(WS, Players),
           catch(ws_close(WS, 1000, 'kicked'), _, true)),
    forall(member(WS, Specs),
           catch(ws_close(WS, 1000, 'kicked'), _, true)).

% The session token of a connected socket, if recorded. Uses ws_token
% (recorded at connect time): looking up session(Seat, Token) by seat
% position instead matches stale sessions of players who already left.
kicked_token(WS, Token) :-
    ws_token(WS, Token),
    !.
kicked_token(_, none).

% A new game can be requested by anyone at any time: it restarts the game
% with the currently seated players (2-4). Departed seats are pruned first,
% so the player count reflects actually-connected players. Spectators
% cannot start games: only seated players control the table.
handle_new_game(WS) :-
    players(Players),
    (   memberchk(WS, Players)
    ->  prune_left_seats,
        players(Seated),
        (   length(Seated, N), N >= 2
        ->  start_new_game
        ;   send_json(WS, json([type=error, message=not_enough_players]))
        )
    ;   send_json(WS, json([type=error, message=not_a_player]))
    ).

% "Start over" (from the game-over screen): like a new game, but the
% scoreboard and the previous-winner lead are wiped too, so the next
% game starts completely fresh. Spectators cannot start games.
handle_start_over(WS) :-
    players(Players),
    (   memberchk(WS, Players)
    ->  prune_left_seats,
        players(Seated),
        (   length(Seated, N), N >= 2
        ->  retractall(last_scoreboard(_)), asserta(last_scoreboard([])),
            retractall(previous_winner(_, _)), asserta(previous_winner(none, none)),
            start_new_game
        ;   send_json(WS, json([type=error, message=not_enough_players]))
        )
    ;   send_json(WS, json([type=error, message=not_a_player]))
    ).

game_is_over(game_state(_, _, _, _, _, _, next_move(_, game_over))).

% ---------------------------------------------------------------------------
% Moves
% ---------------------------------------------------------------------------

handle_move(WS, Dict) :-
    players(Players),
    (   nth0(Seat, Players, WS)          % sender is a seated player
    ->  retractall(grace_flaps(Seat, _)),  % a real move: the human is there
        (   game(GS)
        ->  (   extract_player_index(GS, Seat)    % fails unless it's this seat's turn
            ->  (   client_action_allowed(GS, Dict),
                    apply_move(Dict, GS, NewGS)
                ->  (   Dict.action == pass
                    ->  broadcast_event(passed, Seat)   % animate the pass
                    ;   bomb_beaten(GS, Dict)
                    ->  broadcast_event(bombed, Seat)   % a 2 was bombed!
                    ;   true
                    ),
                    update_game(NewGS)
                ;   send_json(WS, json([type=error, message=illegal_move]))
                )
            ;   send_json(WS, json([type=error, message=not_your_turn]))
            )
        ;   send_json(WS, json([type=error, message=no_game]))
        )
    ;   send_json(WS, json([type=error, message=not_a_player]))
    ).

% True when the move bombs a 2: the placed combination is a four-of-a-kind
% or a double sequence beating a single 2 or pair of 2s (the engine's
% special "bomb" rules).
bomb_beaten(GS, Dict) :-
    GS = game_state(_, _, CardsInPlay, _, _, _, _),
    Dict.action \== pass,
    place_term(Dict.action.place, Place),
    bombing_combination(CardsInPlay, Place).

% Four of a kind bombs a single 2; double sequences bomb 2s per length:
% any length for a single 2, exactly 8 for a pair, exactly 10 for a triple.
bombing_combination(single(card(2, _)), four_of_kind(_)).
bombing_combination(single(card(2, _)), double_sequence(_)).
bombing_combination(pair([card(2, _), card(2, _)]), double_sequence(Cards)) :-
    length(Cards, 8).
bombing_combination(three_of_kind([card(2, _), card(2, _), card(2, _)]),
                     double_sequence(Cards)) :-
    length(Cards, 10).

% A transient event (not game state): clients animate it and discard it.
broadcast_event(Event, Seat) :-
    players(Players),
    spectators(Specs),
    forall((member(WS, Players) ; member(WS, Specs)),
           send_json(WS, json([type=event, event=Event, seat=Seat]))).

% The engine's next_move tells us what the current player must do:
%   make_move -> the player freely chooses (place anything valid or pass)
%   pass      -> the engine decided this player cannot beat: they must
%                confirm by pressing Pass themselves (no auto-skip: skipping
%                would leak to the attacker that nobody could beat)
%   game_over -> nothing to do
% The forced opening place is settled by the server (no choice involved).
% A pass is only legal when there are cards in play: on an empty table the
% player has the lead and must place.
client_action_allowed(GS, Dict) :-
    GS = game_state(_, _, none, _, _, _, next_move(_, make_move)), !,
    Dict.action \== pass.
client_action_allowed(GS, _) :-
    GS = game_state(_, _, CardsInPlay, _, _, _, next_move(_, make_move)),
    CardsInPlay \== none.
client_action_allowed(GS, Dict) :-
    GS = game_state(_, _, _, _, _, _, next_move(_, pass)),
    Dict.action == pass.

apply_move(Dict, GS, NewGS) :-
    (   Dict.action == pass
    ->  pass_move(GS, GS1),
        interpret_tienlen(GS1, GS2),    % advance past the player's pass
        settle(GS2, NewGS)
    ;   place_term(Dict.action.place, Place),
        simulate_placing_cards(Place, GS, GS1),
        settle(GS1, NewGS)
    ).

% A pass on a make_move state is the player's choice; on a pass state the
% engine already decided they must pass. Either way the pass is advanced
% immediately - settle must not stop on the acting player's own pass.
pass_move(GS, GS) :-
    GS = game_state(_, _, _, _, _, _, next_move(_, pass)), !.
pass_move(GS, GS1) :-
    simulate_pass_move(GS, GS1).

% interpret_tienlen auto-plays the forced opening place. Keep interpreting
% until the state is stable: a state where a human must act - a free choice
% (make_move), a forced pass awaiting the player's confirmation (pass) -
% or the round ended (game_over). Stopping at game_over matters:
% interpreting a game_over state deals a whole new round.
settle(GS, GS) :- stable(GS), !.
settle(GS0, GS) :-
    interpret_tienlen(GS0, GS1),
    settle(GS1, GS).

stable(game_state(_, _, _, _, _, _, next_move(_, make_move))).
stable(game_state(_, _, _, _, _, _, next_move(_, pass))).
stable(game_state(_, _, _, _, _, _, next_move(_, game_over))).

% Convert the client's JSON place object into the engine's place term.
place_term(Dict, single(card(Rank, Suit))) :-
    Dict.type == single, !,
    get_dict(rank, Dict.card, Rank),
    get_dict(suit, Dict.card, Suit).
place_term(Dict, pair(Cards)) :-
    Dict.type == pair, !,
    maplist(card_term, Dict.cards, Cards).
place_term(Dict, three_of_kind(Cards)) :-
    Dict.type == three_of_kind, !,
    maplist(card_term, Dict.cards, Cards).
place_term(Dict, four_of_kind(Cards)) :-
    Dict.type == four_of_kind, !,
    maplist(card_term, Dict.cards, Cards).
place_term(Dict, sequence(Cards)) :-
    Dict.type == sequence, !,
    maplist(card_term, Dict.cards, Cards).
place_term(Dict, double_sequence(Cards)) :-
    Dict.type == double_sequence, !,
    maplist(card_term, Dict.cards, Cards).

card_term(Dict, card(Rank, Suit)) :-
    get_dict(rank, Dict, Rank),
    get_dict(suit, Dict, Suit).

% ---------------------------------------------------------------------------
% Game start / update / broadcast
% ---------------------------------------------------------------------------

% The winner of the previous round (if the same players continue): they
% lead the next round with any valid hand instead of the forced 3♠.
% Stored as winner(Seat, Socket): the socket must still hold the seat when
% the next round starts, otherwise seats shifted and nobody earned the lead.
:- dynamic previous_winner/2.
previous_winner(none, none).

start_new_game :-
    % Drop any mid-game departure placeholders FIRST so the game is dealt
    % for the connected players only (seats re-align to connection order).
    prune_left_seats,
    % A new game clears the kick bans: everyone may seat again.
    retractall(kicked_session(_)),
    players(Players),
    length(Players, N),
    (   N == 2 -> initialize_game_two_players(GS)
    ;   N == 3 -> initialize_game_three_players(GS)
    ;   initialize_game_full_players(GS)
    ),
    format(user_error, 'Starting ~w-player game~n', [N]),
    % First round ever (or the player set changed): forced 3♠ opening,
    % settled so clients immediately see a make_move state. Otherwise the
    % previous winner leads the new round with free choice.
    (   previous_winner(Winner, WinnerSocket), Winner \== none, Winner < N,
        nth0(Winner, Players, WinnerSocket)      % same player still in the seat
    ->  GS = game_state(Hands, PlayerStates, _, _, _, _, _),
        % Carry the scoreboard over: placements accumulate across rounds
        % (the engine's own continuation keeps it too).
        (   game(OldGS), OldGS = game_state(_, _, _, _, _, OldSB, _)
        ->  true
        ;   OldSB = []
        ),
        NewGS = game_state(Hands, PlayerStates, none, [], none, OldSB,
                           next_move(Winner, make_move)),
        format(user_error, 'Winner ~w leads the new round~n', [Winner]),
        update_game(NewGS)
    ;   settle(GS, SettledGS),
        update_game(SettledGS)
    ),
    % The winner-lead is consumed: the next deal starts fresh unless a
    % game ends naturally again.
    retractall(previous_winner(_, _)), asserta(previous_winner(none, none)).

% Remove departed players' socket placeholders so the next game is dealt
% for the connected players only (seats re-align to connection order).
% Only seats whose socket is actually dead are pruned: a stale left_seats
% entry (e.g. left over from a previous game) must not drop a live player.
prune_left_seats :-
    left_seats(Left),
    (   Left == []
    ->  true
    ;   players(Players),
        include([Seat]>>(nth0(Seat, Players, WS), \+ socket_alive(WS)), Left, Dead),
        (   Dead == []
        ->  true    % nothing to prune (entries were stale)
        ;   exclude_at(Dead, Players, Remaining),
            retractall(players(_)), assertz(players(Remaining)),
            format(user_error, 'Pruned ~w departed seat(s)~n', [Dead])
        ),
        retractall(left_seats(_)), assertz(left_seats([]))
    ).

% Remove the given 0-based indices from a list (sorted descending so each
% removal keeps the remaining indices valid).
exclude_at([], List, List) :- !.
exclude_at(Indices, List, Rest) :-
    sort(Indices, SortedAsc),
    reverse(SortedAsc, SortedDesc),
    foldl(remove_at, SortedDesc, List, Rest).

update_game(GS) :-
    retractall(game(_)),
    asserta(game(GS)),
    save_room_state,
    % Remember the winner so the next round is led by them (free choice)
    % instead of the forced 3♠ opening.
    (   GS = game_state(_, _, _, _, _, _, next_move(Winner, game_over))
    ->  players(Players),
        (   nth0(Winner, Players, WinnerSocket)
        ->  true
        ;   WinnerSocket = none
        ),
        retractall(previous_winner(_, _)), asserta(previous_winner(Winner, WinnerSocket))
    ;   true
    ),
    % Keep the last scoreboard around after the game is cleared (e.g.
    % the back-to-lobby transition) so the admin view can still show it.
    (   GS = game_state(_, _, _, _, _, SB, _), SB \== []
    ->  retractall(last_scoreboard(_)), asserta(last_scoreboard(SB))
    ;   true
    ),
    broadcast(GS).

% Each player sees their own hand; everyone else's is hidden (one null per
% card, so the client can render the right number of card backs). The view
% also carries your_seat so the client knows which hand is theirs.
broadcast(GS) :-
    players(Players),
    forall(nth0(Seat, Players, WS),
           (   player_view(GS, Seat, View),
               send_json(WS, View)
           )),
    % Spectators get the state too (all hands hidden).
    spectators(Specs),
    forall(member(WS, Specs),
           (   player_view(GS, -1, View),
               send_json(WS, View)
           )).

player_view(GS, Seat, json(Pairs)) :-
    hide_hands(GS, Seat, ViewGS),
    json_convert:prolog_to_json(ViewGS, json(Pairs0), server),
    % seated: how many players hold seats (the client shows the join
    % button to spectators only while there is room).
    players(Players),
    length(Players, Seated),
    append(Pairs0, [your_seat=Seat, seated=Seated, persistent=persistent_mode], Pairs).

hide_hands(game_state(Hands, PlayerStates, CIP, DC, A, SB, NM), Seat,
           game_state(ViewHands, PlayerStates, CIP, DC, A, SB, NM)) :-
    findall(ViewHand,
            (   nth0(I, Hands, Hand),
                hidden_hand(I, Seat, Hand, ViewHand)
            ),
            ViewHands).

hidden_hand(Seat, Seat, Hand, Hand) :- !.
hidden_hand(_, _, Hand, Hidden) :-
    length(Hand, N),
    findall(null, between(1, N, _), Hidden).

% Sockets known to be dead: sending failed once; skip silently after.
:- dynamic dead_socket/1.

send_json(WS, JSON) :-
    (   dead_socket(WS)
    ->  true                                    % already dead: skip quietly
    ;   catch(ws_send(WS, json(JSON)), E,
              (   format(user_error, 'Send failed: ~q~n', [E]),
                  assertz(dead_socket(WS))       % remember: don't retry
              ))
    ).

% ---------------------------------------------------------------------------
% Helpers
% ---------------------------------------------------------------------------

extract_player_index(game_state(_, _, _, _, _, _, next_move(PlayerIndex, _)), PlayerIndex).
