% Integration tests for the server's move handling (no real sockets needed).
%
% These drive the same predicates the WebSocket layer calls (apply_move,
% place_term, settle, handle_move, start_new_game), so the JSON->term
% conversion and the simulate/settle/interpret loop are covered end to end.
%
% The server settles the engine's forced states (forced passes, the forced
% opening place) itself, so after every apply_move the game must be in a
% stable state: next_move(_, make_move) or next_move(_, game_over).
%
% Run: swipl -s server_tests.pl -g "run_tests." -t halt
%   (initialization(start, main) does not fire when loaded with -s)

:- use_module(server).
:- use_module(tienlen).
:- use_module(library(plunit)).
:- use_module(library(http/json)).

% ---------------------------------------------------------------------------
% Helpers to build client move dicts exactly like client.js does
% ---------------------------------------------------------------------------

move_dict(pass, D) :- atom_json_dict('{"type":"move","action":"pass"}', D, [value_string_as(atom)]).

move_dict(place, D) :-
    atom_json_dict('{"type":"move","action":{"place":{"type":"single","card":{"rank":3,"suit":"hearts"}}}}', D, [value_string_as(atom)]).

pair_move_dict(D) :-
    atom_json_dict('{"type":"move","action":{"place":{"type":"pair","cards":[{"rank":3,"suit":"hearts"},{"rank":3,"suit":"clubs"}]}}}', D, [value_string_as(atom)]).

sequence_move_dict(D) :-
    atom_json_dict('{"type":"move","action":{"place":{"type":"sequence","cards":[{"rank":5,"suit":"hearts"},{"rank":6,"suit":"clubs"},{"rank":7,"suit":"spades"}]}}}', D, [value_string_as(atom)]).

% ---------------------------------------------------------------------------
% Tests
% ---------------------------------------------------------------------------

:- begin_tests(server_moves).

test(place_term_single) :-
    move_dict(place, D),
    place_term(D.action.place, single(card(3, hearts))).

test(place_term_pair) :-
    pair_move_dict(D),
    place_term(D.action.place, pair([card(3,hearts), card(3,clubs)])).

test(place_term_sequence) :-
    sequence_move_dict(D),
    place_term(D.action.place, sequence([card(5,hearts), card(6,clubs), card(7,spades)])).

% A voluntary pass by the attacker's opponent returns the turn to the
% attacker and clears the pile (settle plays the "pass clears" rule).
test(apply_move_pass_advances) :-
    GS = game_state([[card(3,spades)],[card(4,hearts)]], [in_play,in_play],
                    single(card(3,hearts)), [], 0, [], next_move(1, make_move)),
    move_dict(pass, D),
    apply_move(D, GS, NewGS),
    get_cards_in_play(NewGS, none),
    get_next_move(NewGS, next_move(0, make_move)).

% A place on an empty table (no cards in play) puts the cards in play and
% hands the turn to the next player.
test(apply_move_place_advances) :-
    GS = game_state([[card(4,spades)],[card(3,hearts)]], [in_play,in_play],
                    none, [], none, [], next_move(1, make_move)),
    move_dict(place, D),
    apply_move(D, GS, NewGS),
    get_cards_in_play(NewGS, single(card(3,hearts))),
    get_next_move(NewGS, next_move(0, make_move)).

% The full loop: place, beat, pass, pile clears back to the attacker.
test(full_loop_place_beat_pass_clear) :-
    GS = game_state([[card(4,spades),card(5,clubs)],[card(3,hearts),card(2,diamonds)]],
                    [in_play,in_play], none, [], none, [], next_move(1, make_move)),
    move_dict(place, D),
    apply_move(D, GS, GS1),
    get_cards_in_play(GS1, single(card(3,hearts))),
    get_next_move(GS1, next_move(0, make_move)),
    % player 0 beats the 3 of hearts with the 4 of spades
    atom_json_dict('{"type":"move","action":{"place":{"type":"single","card":{"rank":4,"suit":"spades"}}}}', D2, [value_string_as(atom)]),
    apply_move(D2, GS1, GS2),
    get_cards_in_play(GS2, single(card(4,spades))),
    get_next_move(GS2, next_move(1, make_move)),
    % player 1 voluntarily passes: pile clears, attacker (player 0) moves
    move_dict(pass, D3),
    apply_move(D3, GS2, GS3),
    get_cards_in_play(GS3, none),
    get_next_move(GS3, next_move(0, make_move)).

% After a place the opponent cannot beat, settle must STOP at the forced
% pass: the player must confirm the pass themselves (no auto-skip, which
% would leak that nobody could beat the cards).
test(settle_stops_at_forced_pass) :-
    GS = game_state([[card(3,spades)],[card(4,hearts),card(5,hearts)]], [in_play,in_play],
                    single(card(3,hearts)), [single(card(2,clubs))], 0, [[first,second]],
                    next_move(1, make_move)),
    atom_json_dict('{"type":"move","action":{"place":{"type":"single","card":{"rank":4,"suit":"hearts"}}}}', D, [value_string_as(atom)]),
    apply_move(D, GS, NewGS),
    get_cards_in_play(NewGS, single(card(4,hearts))),
    get_next_move(NewGS, next_move(0, pass)),
    NewGS = game_state(Hands, _, _, _, _, _, _),
    nth0(1, Hands, [card(5,hearts)]).

% The forced pass is confirmed by the player: their pass advances the game
% and clears the pile back to the attacker.
test(forced_pass_confirmed_by_player) :-
    GS = game_state([[card(3,spades)],[card(4,hearts),card(5,hearts)]], [in_play,in_play],
                    single(card(4,hearts)), [single(card(2,clubs))], 1, [[first,second]],
                    next_move(0, pass)),
    move_dict(pass, D),
    apply_move(D, GS, NewGS),
    get_cards_in_play(NewGS, none),
    get_next_move(NewGS, next_move(1, make_move)).

% When the pile cleared back to the attacker (empty table, their turn),
% passing is illegal: they have the lead and must place.
test(pass_on_empty_table_rejected) :-
    GS = game_state([[card(3,spades)],[card(4,hearts)]], [in_play,in_play],
                    none, [], 0, [], next_move(0, make_move)),
    move_dict(pass, D),
    \+ client_action_allowed(GS, D).

% With cards in play, a voluntary pass on make_move is legal.
test(pass_with_cards_in_play_allowed) :-
    GS = game_state([[card(3,spades)],[card(4,hearts)]], [in_play,in_play],
                    single(card(4,hearts)), [], 1, [], next_move(0, make_move)),
    move_dict(pass, D),
    client_action_allowed(GS, D).

% Multi-player rule: when the first player sheds their last card in a
% 3-player game, the game must CONTINUE 1-on-1 (not end).
test(three_players_continue_after_first_out) :-
    GS = game_state(
        [[card(2,spades)], [card(3,hearts),card(4,hearts)], [card(5,clubs),card(6,clubs)]],
        [in_play,in_play,in_play],
        single(card(3,spades)), [], 0, [],
        next_move(0, make_move)),
    atom_json_dict('{"type":"move","action":{"place":{"type":"single","card":{"rank":2,"suit":"spades"}}}}', D, [value_string_as(atom)]),
    apply_move(D, GS, NewGS),
    \+ game_is_over(NewGS),
    NewGS = game_state(_, PS, _, _, _, _, _),
    PS = [first, in_play, in_play].

% The 1-on-1 continuation plays out: passes clear the pile back to the
% remaining attacker, still not game over.
test(three_players_one_on_one_continues) :-
    GS = game_state(
        [[], [card(3,hearts),card(4,hearts)], [card(5,clubs),card(6,clubs)]],
        [first,in_play,in_play],
        single(card(2,spades)), [], 0, [],
        next_move(1, pass)),
    move_dict(pass, D),
    apply_move(D, GS, GS2),
    apply_move(D, GS2, GS3),
    \+ game_is_over(GS3),
    get_cards_in_play(GS3, none),
    get_next_move(GS3, next_move(1, make_move)).

% Game over only when exactly one player still holds cards: the last
% placement must be recorded for everyone.
test(three_players_game_over_when_one_left) :-
    GS = game_state(
        [[], [card(2,spades)], [card(3,hearts),card(4,clubs)]],
        [first,in_play,in_play],
        single(card(5,diamonds)), [], 0, [],
        next_move(1, make_move)),
    atom_json_dict('{"type":"move","action":{"place":{"type":"single","card":{"rank":2,"suit":"spades"}}}}', D, [value_string_as(atom)]),
    apply_move(D, GS, NewGS),
    game_is_over(NewGS),
    NewGS = game_state(_, PS, _, _, _, _, _),
    PS = [first, second, third].

% A player leaving mid-game must not stall the game: their hand is
% emptied, they get the next placement, and if it was their turn the
% game advances.
test(player_leaves_mid_game_advances) :-
    GS = game_state(
        [[card(3,spades)], [card(4,hearts),card(5,hearts)], [card(6,clubs),card(7,clubs)]],
        [in_play,in_play,in_play],
        single(card(2,diamonds)), [], 0, [],
        next_move(0, make_move)),
    catch(handle_player_left(0, GS), _, true),
    game(NewGS),
    \+ game_is_over(NewGS),
    NewGS = game_state(Hands, States, _, _, _, _, NM),
    nth0(0, Hands, []),
    nth0(0, States, first),
    NM = next_move(P, _),
    P \= 0,   % turn moved off the departed player
    cleanup_fake_game.

% A player leaving when only one other player has cards must end the
% game (everyone gets a placement).
test(player_leaves_ends_game_when_one_left) :-
    GS = game_state(
        [[card(3,spades)], [card(4,hearts)]],
        [in_play,in_play],
        single(card(2,diamonds)), [], 0, [],
        next_move(0, make_move)),
    catch(handle_player_left(0, GS), _, true),
    game(NewGS),
    game_is_over(NewGS),
    NewGS = game_state(_, States, _, _, _, _, _),
    States = [first, second],
    cleanup_fake_game.

% When the attacker with the lead (empty table) leaves, the next player
% with cards must get the lead - not the departed player's seat.
test(attacker_with_lead_leaves_next_player_leads) :-
    server:retractall(players(_)), server:assertz(players([p0, p1, p2])),
    server:retractall(game(_)),
    server:retractall(left_seats(_)), server:assertz(left_seats([])),
    Hands = [
        [card(3,spades),card(4,hearts)],
        [card(6,clubs),card(7,clubs)],
        [card(9,diamonds),card(10,diamonds)]
    ],
    GS = game_state(Hands, [in_play,in_play,in_play],
                    none, [single(card(5,hearts))], 1, [],
                    next_move(1, make_move)),
    catch(handle_player_left(1, GS), _, true),
    game(NewGS),
    get_next_move(NewGS, next_move(2, make_move)),   % P3 leads, not the leaver
    get_cards_in_play(NewGS, none),
    cleanup_fake_game.

% After a game ends naturally, the next round must be led by the winner
% with free choice (make_move on an empty table), not the forced 3♠.
% The scoreboard must accumulate across rounds.
test(winner_leads_next_round) :-
    server:retractall(players(_)), server:assertz(players([fake_ws_0, fake_ws_1])),
    server:retractall(game(_)),
    server:retractall(previous_winner(_, _)), server:asserta(previous_winner(none, none)),
    % start a first game (forced opening)
    catch(start_new_game, _, true),
    game(GS1),
    get_next_move(GS1, next_move(_, make_move)),
    % end it naturally with player 1 winning
    GS2 = game_state(_, _, _, _, _, [[first,second]], next_move(1, game_over)),
    catch(update_game(GS2), _, true),
    % next round: winner leads with free choice, scoreboard carried over
    catch(start_new_game, _, true),
    game(GS3),
    get_next_move(GS3, next_move(1, make_move)),
    get_cards_in_play(GS3, none),
    GS3 = game_state(_, _, _, _, _, SB3, _),
    SB3 == [[first,second]],
    cleanup_fake_game.

% A mid-game restart must NOT give the winner the lead (round unfinished).
test(restart_mid_game_no_winner_lead) :-
    server:retractall(players(_)), server:assertz(players([fake_ws_0, fake_ws_1])),
    server:retractall(game(_)),
    server:retractall(previous_winner(_, _)), server:asserta(previous_winner(none, none)),
    catch(start_new_game, _, true),
    % restart mid-game (no natural game over)
    catch(start_new_game, _, true),
    game(GS),
    % forced opening was settled: a card is in play
    get_cards_in_play(GS, single(_)),
    cleanup_fake_game.

% "Start over" from the game-over screen: the scoreboard and the
% previous-winner lead are wiped, so the next game starts completely
% fresh (forced opening, empty scoreboard).
test(start_over_wipes_scoreboard_and_lead) :-
    server:retractall(players(_)), server:assertz(players([fake_ws_0, fake_ws_1])),
    server:retractall(game(_)),
    server:retractall(previous_winner(_, _)), server:asserta(previous_winner(none, none)),
    catch(start_new_game, _, true),
    % end it naturally with player 1 winning
    GS2 = game_state(_, _, _, _, _, [[first,second]], next_move(1, game_over)),
    catch(update_game(GS2), _, true),
    server:last_scoreboard(LSB0), LSB0 == [[first,second]],
    % start over via the actual handler (fake socket: sends are caught)
    catch(server:handle_start_over(fake_ws_0), _, true),
    game(GS3),
    get_next_move(GS3, next_move(_, make_move)),
    get_cards_in_play(GS3, single(_)),   % forced opening again
    GS3 = game_state(_, _, _, _, _, SB3, _),
    SB3 == [],
    server:last_scoreboard(LSB), LSB == [],
    cleanup_fake_game.

% "Start over" with too few seated players is rejected with an error
% (same guard as new_game).
test(start_over_needs_two_players) :-
    server:retractall(players(_)), server:assertz(players([fake_ws_0])),
    server:retractall(game(_)),
    catch(server:handle_start_over(fake_ws_0), _, true),
    \+ server:game(_),
    cleanup_fake_game.

% Spectators cannot start games or start over: only seated players
% control the table.
test(spectator_cannot_start_games) :-
    server:retractall(players(_)), server:assertz(players([fake_ws_0, fake_ws_1])),
    server:retractall(spectators(_)), server:assertz(spectators([spec_ws])),
    server:retractall(game(_)),
    catch(handle_message(spec_ws, '{"type":"new_game"}'), _, true),
    \+ server:game(_),                          % no game started
    catch(handle_message(spec_ws, '{"type":"start_over"}'), _, true),
    \+ server:game(_),
    cleanup_fake_game.

% The admin restart thread inherits the HTTP worker's client socket as
% current_output, which closes once the reply is sent. The engine's
% writeln/1 debug output must not crash the thread before the game is
% dealt: with_null_output redirects it to a null stream.
test(admin_restart_thread_survives_closed_output) :-
    server:retractall(players(_)), server:assertz(players([fake_ws_0, fake_ws_1])),
    server:retractall(game(_)),
    server:retractall(previous_winner(_, _)), server:asserta(previous_winner(none, none)),
    % Simulate the inherited closed socket: current_output is a stream
    % that another thread closes before the restart thread runs.
    setup_call_cleanup(
        (   tmp_file_stream(text, Path, S),
            set_output(S)
        ),
        (   thread_create(server:admin_restart_thread, Id, []),
            sleep(0.05),
            close(S),
            thread_join(Id, _)
        ),
        (   set_output(user), delete_file(Path)
        )),
    % The thread must have dealt a fresh game despite the closed output.
    game(GS),
    get_next_move(GS, next_move(_, make_move)),
    cleanup_fake_game.

% Turn validation: extract_player_index doubles as "is it this seat's turn".
test(turn_validation) :-
    GS = game_state([[card(3,spades)],[card(4,hearts)]], [in_play,in_play], none, [], none, [], next_move(1, make_move)),
    extract_player_index(GS, 1),
    \+ extract_player_index(GS, 0).

test(game_is_over_detection) :-
    game_is_over(game_state([[],[]], [first,second], none, [], none, [], next_move(0, game_over))),
    \+ game_is_over(game_state([[card(3,spades)],[card(4,hearts)]], [in_play,in_play], none, [], none, [], next_move(1, make_move))).

% ---------------------------------------------------------------------------
% handle_move / start_new_game (fake sockets: send_json catches ws_send
% failures on non-streams, so broadcast is safe in tests)
% ---------------------------------------------------------------------------

test(handle_move_wrong_seat_rejected) :-
    setup_fake_game(next_move(1, make_move)),
    move_dict(pass, D),
    handle_move(fake_ws_0, D),
    % state must be unchanged: it was player 1's turn, not seat 0's
    game(GS),
    get_next_move(GS, next_move(1, make_move)),
    cleanup_fake_game.

test(handle_move_right_seat_advances) :-
    setup_fake_game(next_move(1, make_move)),
    move_dict(pass, D),
    handle_move(fake_ws_1, D),
    game(GS),
    get_next_move(GS, next_move(0, make_move)),
    cleanup_fake_game.

test(handle_move_illegal_place_rejected) :-
    setup_fake_game(next_move(1, make_move)),
    % player 1 does not hold the 10 of spades
    atom_json_dict('{"type":"move","action":{"place":{"type":"single","card":{"rank":10,"suit":"spades"}}}}', D, [value_string_as(atom)]),
    handle_move(fake_ws_1, D),
    game(GS),
    get_next_move(GS, next_move(1, make_move)),
    cleanup_fake_game.

% start_new_game must settle the forced opening: clients see a make_move
% state with the lowest card already on the table. The deal is random, so
% we only check that *some* single card was auto-placed and removed from
% the opener's hand (the next player can always beat the global lowest).
test(start_new_game_settles_opening) :-
    server:retractall(players(_)), server:assertz(players([fake_ws_0, fake_ws_1])),
    server:retractall(previous_winner(_, _)), server:asserta(previous_winner(none, none)),
    catch(start_new_game, _, true),
    game(GS),
    get_next_move(GS, next_move(_, make_move)),
    get_cards_in_play(GS, single(Card)),
    GS = game_state(Hands, _, _, _, _, _, _),
    append(Hands, AllCards),
    \+ memberchk(Card, AllCards),
    cleanup_fake_game.

% The lobby: connections land as spectators (joining is an explicit
% choice via the join message); no game starts and no seat is taken.
test(lobby_no_autostart) :-
    server:retractall(players(_)), server:assertz(players([])),
    server:retractall(game(_)),
    catch(seat_client(fake_ws_0, none, none, 0), _, true),
    catch(seat_client(fake_ws_1, none, none, 0), _, true),
    catch(seat_client(fake_ws_2, none, none, 0), _, true),
    \+ game(_),
    players([]),
    server:spectators(Specs),
    memberchk(fake_ws_0, Specs),
    memberchk(fake_ws_1, Specs),
    memberchk(fake_ws_2, Specs),
    cleanup_fake_game.

% A new_game request with 3 seated players must start a 3-player game.
test(new_game_with_three_players) :-
    server:retractall(players(_)), server:assertz(players([fake_ws_0, fake_ws_1, fake_ws_2])),
    server:retractall(game(_)),
    server:retractall(previous_winner(_, _)), server:asserta(previous_winner(none, none)),
    catch(start_new_game, _, true),
    game(GS),
    GS = game_state(Hands, _, _, _, _, _, _),
    length(Hands, 3),
    get_next_move(GS, next_move(_, make_move)),
    cleanup_fake_game.

% A new_game request with 4 seated players must start a 4-player game.
test(new_game_with_four_players) :-
    server:retractall(players(_)), server:assertz(players([fake_ws_0, fake_ws_1, fake_ws_2, fake_ws_3])),
    server:retractall(game(_)),
    server:retractall(previous_winner(_, _)), server:asserta(previous_winner(none, none)),
    catch(start_new_game, _, true),
    game(GS),
    GS = game_state(Hands, _, _, _, _, _, _),
    length(Hands, 4),
    get_next_move(GS, next_move(_, make_move)),
    cleanup_fake_game.

% A new_game request during a running game must restart it: fresh hands,
% fresh scoreboard, back to a settled make_move state.
test(new_game_restarts_mid_game) :-
    setup_fake_game(next_move(1, make_move)),
    catch(start_new_game, _, true),
    game(GS),
    get_next_move(GS, next_move(_, make_move)),
    get_cards_in_play(GS, single(_)),   % settled opening already placed
    GS = game_state(Hands, _, _, _, _, SB, _),
    SB == [],                            % scoreboard reset
    maplist(\=([]), Hands),              % fresh full hands
    cleanup_fake_game.

% kick_all must clear the lobby and the game so everyone can reseat.
test(kick_all_clears_lobby_and_game) :-
    setup_fake_game(next_move(1, make_move)),
    catch(handle_kick_all, _, true),
    players([]),
    \+ game(_),
    cleanup_fake_game.

% A reconnecting player (same session token, dead old socket) reclaims
% their seat instead of being seated as a new player.
test(reconnect_reclaims_seat) :-
    server:retractall(players(_)), server:assertz(players([old_ws, fake_ws_1])),
    server:retractall(game(_)),
    server:retractall(session(_, _)),
    server:assertz(session(0, token_a)),
    catch(seat_client(new_ws, token_a, none, 0), _, true),
    players([new_ws, fake_ws_1]),   % seat 0 replaced, not appended
    server:session(0, token_a),     % session still registered
    cleanup_fake_game.

% A kicked client reconnects as a spectator (auto-seat banned), then the
% "join" message seats them and lifts the ban: the manual escape from
% the kick-all deadlock.
test(join_after_kick_seats_and_lifts_ban) :-
    server:retractall(players(_)), server:assertz(players([])),
    server:retractall(spectators(_)), server:assertz(spectators([])),
    server:retractall(game(_)),
    server:retractall(session(_, _)),
    server:retractall(kicked_session(_)),
    server:retractall(ws_token(_, _)),
    server:assertz(kicked_session(token_k)),
    catch(seat_client(kicked_ws, token_k, none, 0), _, true),   % reconnect: spectator
    server:spectators(Specs1), memberchk(kicked_ws, Specs1),
    catch(handle_message(kicked_ws, '{"type":"join"}'), _, true),
    players([kicked_ws]),                    % seated via the join message
    \+ server:kicked_session(token_k),        % ban lifted
    server:session(0, token_k),               % session registered
    cleanup_fake_game.

% The join message is only for spectators.
test(join_rejected_for_seated_player) :-
    server:retractall(players(_)), server:assertz(players([fake_ws_0])),
    server:retractall(spectators(_)), server:assertz(spectators([])),
    server:retractall(game(_)),
    catch(handle_message(fake_ws_0, '{"type":"join"}'), _, true),
    players([fake_ws_0]),                     % unchanged
    cleanup_fake_game.

% A fresh connection is a spectator; the explicit join message seats
% them (joining is a deliberate choice, not automatic).
test(fresh_connection_spectator_until_join) :-
    server:retractall(players(_)), server:assertz(players([])),
    server:retractall(spectators(_)), server:assertz(spectators([])),
    server:retractall(game(_)),
    server:retractall(session(_, _)),
    server:retractall(ws_token(_, _)),
    catch(seat_client(fresh_ws, token_fresh, none, 0), _, true),
    players([]),                              % not seated automatically
    server:spectators(Specs), memberchk(fresh_ws, Specs),
    catch(handle_message(fresh_ws, '{"type":"join"}'), _, true),
    players([fresh_ws]),                     % seated after the explicit join
    server:session(0, token_fresh),           % session registered
    cleanup_fake_game.

% Joining while a game is running must not reset the game for everyone:
% the joiner is appended (dealt in at the next game) and the game state
% is untouched - no lobby broadcast mid-game.
test(join_mid_game_appends_player_keeps_game) :-
    server:retractall(players(_)), server:assertz(players([fake_ws_0, fake_ws_1])),
    server:retractall(spectators(_)), server:assertz(spectators([joiner_ws])),
    server:retractall(game(_)),
    server:retractall(session(_, _)),
    server:retractall(ws_token(_, _)),
    server:assertz(ws_token(joiner_ws, token_j)),
    server:retractall(left_seats(_)), server:assertz(left_seats([])),
    GS = game_state([[card(3,spades)],[card(4,hearts)]], [in_play,in_play],
                    single(card(3,hearts)), [], 0, [], next_move(0, make_move)),
    server:assertz(game(GS)),
    catch(handle_message(joiner_ws, '{"type":"join"}'), _, true),
    players([fake_ws_0, fake_ws_1, joiner_ws]),   % appended, not replacing
    server:game(GS2), GS2 =@= GS,                % game state unchanged
    server:spectators([]),                        % no longer a spectator
    server:session(2, token_j),                   % session registered for seat 2
    cleanup_fake_game.

% A joiner takes an open departed seat (recorded in left_seats) instead
% of appending a new one.
test(join_takes_departed_seat) :-
    server:retractall(players(_)), server:assertz(players([fake_ws_0, gone_ws, fake_ws_1])),
    server:retractall(spectators(_)), server:assertz(spectators([joiner_ws])),
    server:retractall(game(_)),
    server:retractall(session(_, _)),
    server:retractall(ws_token(_, _)),
    server:retractall(left_seats(_)), server:assertz(left_seats([1])),
    catch(handle_message(joiner_ws, '{"type":"join"}'), _, true),
    players([fake_ws_0, joiner_ws, fake_ws_1]),    % seat 1 replaced
    server:left_seats([]),                        % seat no longer open
    cleanup_fake_game.

% Declining a seat offer keeps the spectator in the spectators list (they
% keep watching and can still join later); the declination is recorded
% so the same seat is not offered to them again.
test(decline_seat_keeps_spectator) :-
    server:retractall(players(_)), server:assertz(players([fake_ws_0, gone_ws, fake_ws_1])),
    server:retractall(spectators(_)), server:assertz(spectators([spec_ws])),
    server:retractall(game(_)),
    server:retractall(seat_declined(_, _)),
    catch(handle_message(spec_ws, '{"type":"decline_seat","seat":1}'), _, true),
    server:spectators(Specs), memberchk(spec_ws, Specs),  % still watching
    server:seat_declined(1, spec_ws),                     % declination recorded
    cleanup_fake_game.

% Bombs are recorded with sequence ids so a client that was away can
% replay them on reconnect: the away client (last seen 0) is told about
% a newer bomb, the live client (last seen the bomb's id) is not, and
% a new game clears the history.
test(bomb_replay_for_returning_clients) :-
    server:retractall(players(_)), server:assertz(players([fake_ws_0, fake_ws_1])),
    server:retractall(spectators(_)), server:assertz(spectators([])),
    server:retractall(bomb_event(_, _)),
    server:retractall(bomb_seq(_)), server:assertz(bomb_seq(0)),
    server:retractall(game(_)),
    server:retractall(dead_socket(_)),
    broadcast_event(bombed, 0),
    server:bomb_event(Id, 0), Id == 1,
    findall(B, (server:bomb_event(B, _), B > 0), Missed), Missed == [1],
    findall(B, (server:bomb_event(B, _), B > 1), Current), Current == [],
    catch(start_new_game, _, true),
    \+ server:bomb_event(_, _),
    cleanup_fake_game.

% Accepting a seat offer for a seat that no longer exists (the lobby
% compacted after departures) is a clean error, not a crash.
test(accept_seat_out_of_range_is_error) :-
    server:retractall(players(_)), server:assertz(players([fake_ws_0])),
    server:retractall(spectators(_)), server:assertz(spectators([spec_ws])),
    server:retractall(game(_)),
    catch(handle_message(spec_ws, '{"type":"accept_seat","seat":5}'), _, true),
    players([fake_ws_0]),                          % unchanged
    server:spectators(Specs), memberchk(spec_ws, Specs),
    cleanup_fake_game.

% Bomb detection: a four-of-a-kind placed on a single 2 counts.
test(bomb_beaten_four_of_kind_on_single_2) :-
    GS = game_state([[card(3,spades)],[card(4,hearts)]], [in_play,in_play],
                    single(card(2,spades)), [], 1, [], next_move(0, make_move)),
    atom_json_dict('{"type":"move","action":{"place":{"type":"four_of_kind","cards":[{"rank":3,"suit":"hearts"},{"rank":3,"suit":"diamonds"},{"rank":3,"suit":"clubs"},{"rank":3,"suit":"spades"}]}}}', D, [value_string_as(atom)]),
    bomb_beaten(GS, D).

% A double sequence of 8 on a single 2 counts.
test(bomb_beaten_double_sequence_on_single_2) :-
    GS = game_state([[card(3,spades)],[card(4,hearts)]], [in_play,in_play],
                    single(card(2,spades)), [], 1, [], next_move(0, make_move)),
    atom_json_dict('{"type":"move","action":{"place":{"type":"double_sequence","cards":[{"rank":3,"suit":"hearts"},{"rank":3,"suit":"diamonds"},{"rank":4,"suit":"hearts"},{"rank":4,"suit":"diamonds"},{"rank":5,"suit":"hearts"},{"rank":5,"suit":"diamonds"},{"rank":6,"suit":"hearts"},{"rank":6,"suit":"diamonds"}]}}}', D, [value_string_as(atom)]),
    bomb_beaten(GS, D).

% A double sequence of exactly 8 bombs a pair of 2s.
test(bomb_beaten_double_sequence_on_pair_of_2s) :-
    GS = game_state([[card(3,spades)],[card(4,hearts)]], [in_play,in_play],
                    pair([card(2,spades),card(2,hearts)]), [], 1, [], next_move(0, make_move)),
    atom_json_dict('{"type":"move","action":{"place":{"type":"double_sequence","cards":[{"rank":3,"suit":"hearts"},{"rank":3,"suit":"diamonds"},{"rank":4,"suit":"hearts"},{"rank":4,"suit":"diamonds"},{"rank":5,"suit":"hearts"},{"rank":5,"suit":"diamonds"},{"rank":6,"suit":"hearts"},{"rank":6,"suit":"diamonds"}]}}}', D, [value_string_as(atom)]),
    bomb_beaten(GS, D).

% A double sequence of exactly 10 bombs a three of a kind of 2s.
test(bomb_beaten_double_sequence_on_three_2s) :-
    GS = game_state([[card(3,spades)],[card(4,hearts)]], [in_play,in_play],
                    three_of_kind([card(2,spades),card(2,hearts),card(2,diamonds)]), [], 1, [], next_move(0, make_move)),
    atom_json_dict('{"type":"move","action":{"place":{"type":"double_sequence","cards":[{"rank":3,"suit":"hearts"},{"rank":3,"suit":"diamonds"},{"rank":4,"suit":"hearts"},{"rank":4,"suit":"diamonds"},{"rank":5,"suit":"hearts"},{"rank":5,"suit":"diamonds"},{"rank":6,"suit":"hearts"},{"rank":6,"suit":"diamonds"},{"rank":7,"suit":"hearts"},{"rank":7,"suit":"diamonds"}]}}}', D, [value_string_as(atom)]),
    bomb_beaten(GS, D).

% A four-of-a-kind does NOT bomb a pair of 2s (only double sequences do).
test(no_bomb_four_of_kind_on_pair_of_2s) :-
    GS = game_state([[card(3,spades)],[card(4,hearts)]], [in_play,in_play],
                    pair([card(2,spades),card(2,hearts)]), [], 1, [], next_move(0, make_move)),
    atom_json_dict('{"type":"move","action":{"place":{"type":"four_of_kind","cards":[{"rank":3,"suit":"hearts"},{"rank":3,"suit":"diamonds"},{"rank":3,"suit":"clubs"},{"rank":3,"suit":"spades"}]}}}', D, [value_string_as(atom)]),
    \+ bomb_beaten(GS, D).

% A normal place on a 2 (e.g. a higher single) is NOT a bomb.
test(no_bomb_for_normal_beat) :-
    GS = game_state([[card(3,spades)],[card(4,hearts)]], [in_play,in_play],
                    single(card(2,spades)), [], 1, [], next_move(0, make_move)),
    move_dict(pass, D),
    \+ bomb_beaten(GS, D).

% A four-of-a-kind on a non-2 card is NOT a bomb.
test(no_bomb_on_regular_card) :-
    GS = game_state([[card(3,spades)],[card(4,hearts)]], [in_play,in_play],
                    single(card(4,spades)), [], 1, [], next_move(0, make_move)),
    atom_json_dict('{"type":"move","action":{"place":{"type":"four_of_kind","cards":[{"rank":3,"suit":"hearts"},{"rank":3,"suit":"diamonds"},{"rank":3,"suit":"clubs"},{"rank":3,"suit":"spades"}]}}}', D, [value_string_as(atom)]),
    \+ bomb_beaten(GS, D).

% Each player sees their own hand in full; other hands become null
% placeholders (one per card) and the view carries your_seat.
test(player_view_hides_other_hands) :-
    GS = game_state([[card(3,spades)],[card(4,hearts),card(5,hearts)]], [in_play,in_play],
                    single(card(3,hearts)), [], 0, [], next_move(0, make_move)),
    player_view(GS, 0, json(Pairs0)),
    memberchk(hands=[[json([rank=3,suit=spades])],[null,null]], Pairs0),
    memberchk(your_seat=0, Pairs0),
    player_view(GS, 1, json(Pairs1)),
    memberchk(hands=[[null],[json([rank=4,suit=hearts]),json([rank=5,suit=hearts])]], Pairs1),
    memberchk(your_seat=1, Pairs1).

% ---------------------------------------------------------------------------
% Fixtures (assertions must target the server module's dynamic predicates)
% ---------------------------------------------------------------------------

setup_fake_game(NextMove) :-
    server:retractall(players(_)), server:assertz(players([fake_ws_0, fake_ws_1])),
    server:retractall(previous_winner(_, _)), server:asserta(previous_winner(none, none)),
    GS = game_state([[card(3,spades)],[card(4,hearts)]], [in_play,in_play],
                    single(card(3,hearts)), [], 0, [], NextMove),
    server:retractall(game(_)), server:assertz(game(GS)).

cleanup_fake_game :-
    server:retractall(players(_)), server:assertz(players([])),
    server:retractall(game(_)),
    server:retractall(spectators(_)), server:assertz(spectators([])),
    server:retractall(session(_, _)),
    server:retractall(kicked_session(_)),
    server:retractall(ws_token(_, _)),
    server:retractall(seat_declined(_, _)),
    server:retractall(left_seats(_)), server:assertz(left_seats([])),
    server:retractall(dead_socket(_)),
    server:retractall(bomb_event(_, _)),
    server:retractall(bomb_seq(_)), server:assertz(bomb_seq(0)),
    server:retractall(previous_winner(_, _)), server:asserta(previous_winner(none, none)).

:- end_tests(server_moves).
