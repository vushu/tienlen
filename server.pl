:- use_module(library(http/http_server)).
:- use_module(library(http/websocket)).
:- use_module(tienlen).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(record)).

:- initialization(start, main).
:- dynamic room/3.

% Define WebSocket handler
:- http_handler(root(.), http_upgrade_to_websocket(handle_ws(run), []), [spawn([])]).

% Start the WebSocket server on port 8000
start :- http_server([port(8080)]).

room(1, [], none).
room(2, [], none).
room(3, [], none).
room(4, [], none).

% Finding almost full rooms
find_available_room(room(RoomId, FoundClients, GS1)) :-
    findall(room(Id, Clients, GS), (room(Id, Clients, GS), length(Clients, L), L < 4), Rooms),
    sort(2, @>=, Rooms, SortedRooms),
    nth0(0, SortedRooms, room(RoomId, FoundClients, GS1)).

% Stop the WebSocket connection
handle_ws(stop, WebSocket) :- 
    room(ID, Clients, GS), memberchk(WebSocket, Clients), select(WebSocket, Clients, NewClients),
    retract(room(ID, Clients, GS)),
    asserta(room(ID, NewClients, GS)),
    writeln('Client disconnected'),
    print_rooms.

handle_ws(run, WebSocket) :-
    add_client(WebSocket), 
    ws_receive(WebSocket, Msg),
    handle_opcode(WebSocket, Msg, State),
    handle_ws(State, WebSocket).

handle_opcode(WebSocket, Msg, State) :-
    Msg.opcode == close -> State = stop; State = run.

add_client(WebSocket) :-
    room(_, Clients, _), memberchk(WebSocket, Clients),
    format('Client already exists ~n', []).

add_client(WebSocket) :-
    find_available_room(room(ID, Clients, GS)),
    (
        length(Clients, 3) 
            -> format(atom(Message),'Room is getting full now starting game ~w~n', [1]),
                send_message(WebSocket, Message)
            ; format(atom(Mess),'Room is not full waiting for player ~w~n', [WebSocket]), send_message(WebSocket, Mess) 
    ),
    add_client_to_room(ID, WebSocket).

add_client_to_room(ID, WebSocket) :-
    format('Adding client to room ~w~n', [ID]),   
    retract(room(ID, Clients, GS)),
    asserta(room(ID, [WebSocket | Clients], GS)), print_rooms.

print_rooms :- get_all_rooms(Rooms), format("Rooms: ~w~n", [Rooms]).

get_all_rooms(Rooms) :- findall(room(Id, Clients, GS), room(Id, Clients, GS), Rooms).

start_game(Clients) :- 
    length(Clients, L), 
    L >= 4, initialize_game_full_players(GS),
    extract_player_index(GS, Index), prolog_to_json(GS, JSON), nth0(Index, Clients, WS), ws_send(WS, json(JSON)) .

start_game(Clients) :- 
    length(Clients, L), 
    L < 4, writeln('Not enough players'), 
    length(Clients, Len), writeln(Len).

initialize_game(GameState) :- initialize_game_full_players(GameState).

send_message(Client, Message) :- 
    ws_send(Client, json(Message)).

:- json_object
    card(rank: (text;integer), suit: text).

:- json_object
    next_move(player, action: any).

:- json_object
    place(place: any).

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


extract_player_index(game_state(_, _, _, _, _, _, next_move(PlayerIndex, _)), PlayerIndex).

% init_full_player_game(Start,GameStateJson) :- initialize_game_full_players(GS), prolog_to_json(GS, GameStateJson).
% init_three_player_game(Start, GameStateJson) :- initialize_game_three_players(GS), prolog_to_json(GS, GameStateJson).
% init_two_player_game(Start,GameStateJson) :- initialize_game_two_players(GS), prolog_to_json(GS, GameStateJson).

                            % hands      scoreBoard  cardsInPlay  discardedCards, attackerindex, scoreboard nextMove
last_game_state(GS) :-
    GS = game_state([[card(5,spades),card(6,spades),card(6,hearts),card(j,hearts),card(q,clubs),card(q,diamonds),card(q,hearts),card(k,clubs),card(a,hearts)],[card(3,diamonds),card(3,hearts),card(4,clubs),card(4,diamonds),card(7,hearts),card(8,diamonds),card(j,clubs),card(j,diamonds),card(k,diamonds),card(a,spades),card(a,diamonds)],[card(5,clubs),card(6,clubs),card(6,diamonds),card(7,clubs),card(8,clubs),card(8,hearts),card(9,clubs),card(9,hearts),card(10,spades),card(10,diamonds),card(j,spades),card(k,spades)],[card(4,spades),card(5,diamonds),card(7,spades),card(7,diamonds),card(8,spades),card(9,diamonds),card(10,hearts),card(q,spades),card(k,hearts),card(a,clubs),card(2,spades),card(2,clubs)]],[in_play,pass,in_play,in_play],single(card(2,diamonds)),[single(card(3,spades)),single(card(3,clubs)),single(card(4,hearts)),single(card(5,hearts)),single(card(9,spades)),single(card(10,clubs)),single(card(2,hearts))],0,[],next_move(2,make_move)).
say_hi(Message) :- 
    writeln('Connected To TienLen Server'),
    writeln(Message).
%--------------- UNIT TESTS -------------------
:- use_module(library(plunit)).

:- begin_tests(tienlen_server).

% test(test_initialize_game) :- 
    % init_game_state_as_json(GS), assertion(GS = json([hands=[[json([rank=6,suit=clubs]),json([rank=9,suit=hearts]),json([rank=5,suit=spades]),json([rank=2,suit=clubs]),json([rank=j,suit=hearts]),json([rank=10,suit=diamonds]),json([rank=5,suit=hearts]),json([rank=5,suit=diamonds]),json([rank=8,suit=spades]),json([rank=7,suit=clubs]),json([rank=5,suit=clubs]),json([rank=2,suit=diamonds]),json([rank=k,suit=hearts])],[json([rank=9,suit=clubs]),json([rank=3,suit=diamonds]),json([rank=j,suit=diamonds]),json([rank=8,suit=clubs]),json([rank=6,suit=diamonds]),json([rank=3,suit=spades]),json([rank=6,suit=spades]),json([rank=10,suit=spades]),json([rank=8,suit=diamonds]),json([rank=a,suit=diamonds]),json([rank=9,suit=diamonds]),json([rank=j,suit=spades]),json([rank=8,suit=hearts])],[json([rank=10,suit=hearts]),json([rank=j,suit=clubs]),json([rank=9,suit=spades]),json([rank=a,suit=clubs]),json([rank=k,suit=clubs]),json([rank=4,suit=spades]),json([rank=a,suit=spades]),json([rank=q,suit=spades]),json([rank=3,suit=hearts]),json([rank=2,suit=hearts]),json([rank=a,suit=hearts]),json([rank=q,suit=hearts]),json([rank=2,suit=spades])],[json([rank=3,suit=clubs]),json([rank=k,suit=spades]),json([rank=q,suit=diamonds]),json([rank=7,suit=hearts]),json([rank=k,suit=diamonds]),json([rank=10,suit=clubs]),json([rank=7,suit=spades]),json([rank=4,suit=hearts]),json([rank=q,suit=clubs]),json([rank=6,suit=hearts]),json([rank=7,suit=diamonds]),json([rank=4,suit=diamonds]),json([rank=4,suit=clubs])]],player_states=[in_play,in_play,in_play,in_play],cards_in_play=none,discarded_cards=[],attacker=none,scoreboard=[],next_move=json([player=1,action=json([place=json([single=json([rank=3,suit=spades])])])])])).

test(placing_sequence_json) :-
    GS = game_state([
        [card(3,clubs),card(7,spades),card(7,hearts),card(8,clubs),card(q,diamonds),card(k,clubs),card(a,clubs)],
        [],
        [card(5,clubs),card(6,clubs),card(7,clubs),card(10,clubs),card(j,clubs),card(j,hearts),card(q,spades),card(k,spades),card(a,diamonds),card(a,hearts)],
            [card(3,diamonds),card(4,diamonds),card(4,hearts),card(6,hearts),card(7,diamonds),card(8,hearts),card(9,clubs),card(10,hearts),card(j,diamonds),card(k,diamonds)]],
        [in_play,first,in_play,in_play],
        sequence([card(j,spades),card(q,hearts),card(k,hearts),card(a,spades)]),[single(card(3,spades)),single(card(3,hearts)),single(card(4,spades)),single(card(4,clubs)),single(card(10,spades)),single(card(10,diamonds)),single(card(q,clubs)),single(card(2,clubs)),single(card(2,diamonds)),
        three_of_kind([card(5,spades),card(5,diamonds),card(5,hearts)]),three_of_kind([card(9,spades),card(9,diamonds),card(9,hearts)]),pair([card(6,spades),card(6,diamonds)]),pair([card(8,spades),card(8,diamonds)]),pair([card(2,spades),card(2,hearts)])],1,[],next_move(2,place(sequence([card(j,hearts),card(q,spades),card(k,spades),card(a,diamonds)])))),
        prolog_to_json(GS, _).
        % assertion(GSJSON = json([hands=[[json([rank=3,suit=clubs]),json([rank=7,suit=spades]),json([rank=7,suit=hearts]),json([rank=8,suit=clubs]),json([rank=q,suit=diamonds]),json([rank=k,suit=clubs]),json([rank=a,suit=clubs])],[],[json([rank=5,suit=clubs]),json([rank=6,suit=clubs]),json([rank=7,suit=clubs]),json([rank=10,suit=clubs]),json([rank=j,suit=clubs]),json([rank=j,suit=hearts]),json([rank=q,suit=spades]),json([rank=k,suit=spades]),json([rank=a,suit=diamonds]),json([rank=a,suit=hearts])],[json([rank=3,suit=diamonds]),json([rank=4,suit=diamonds]),json([rank=4,suit=hearts]),json([rank=6,suit=hearts]),json([rank=7,suit=diamonds]),json([rank=8,suit=hearts]),json([rank=9,suit=clubs]),json([rank=10,suit=hearts]),json([rank=j,suit=diamonds]),json([rank=k,suit=diamonds])]],player_states=[in_play,first,in_play,in_play],cards_in_play=json([sequence=[json([rank=j,suit=spades]),json([rank=q,suit=hearts]),json([rank=k,suit=hearts]),json([rank=a,suit=spades])]]),discarded_cards=[json([single=json([rank=3,suit=spades])]),json([single=json([rank=3,suit=hearts])]),json([single=json([rank=4,suit=spades])]),json([single=json([rank=4,suit=clubs])]),json([single=json([rank=10,suit=spades])]),json([single=json([rank=10,suit=diamonds])]),json([single=json([rank=q,suit=clubs])]),json([single=json([rank=2,suit=clubs])]),json([single=json([rank=2,suit=diamonds])]),json([three_of_kind=[json([rank=5,suit=spades]),json([rank=5,suit=diamonds]),json([rank=5,suit=hearts])]]),json([three_of_kind=[json([rank=9,suit=spades]),json([rank=9,suit=diamonds]),json([rank=9,suit=hearts])]]),json([pair=[json([rank=6,suit=spades]),json([rank=6,suit=diamonds])]]),json([pair=[json([rank=8,suit=spades]),json([rank=8,suit=diamonds])]]),json([pair=[json([rank=2,suit=spades]),json([rank=2,suit=hearts])]])],attacker=1,scoreboard=[],next_move=json([player=2,action=json([place=json([sequence=[json([rank=j,suit=hearts]),json([rank=q,suit=spades]),json([rank=k,suit=spades]),json([rank=a,suit=diamonds])]])])])])).

% test(run_top_level_test) :-  
    % global_level_test.


:- end_tests(tienlen_server).

global_level_test :-
    retractall(room(_, _, _)),
    assertz(room(1, [1,3,4], none)),
    assertz(room(2, [], none)),
    assertz(room(3, [], none)),
    assertz(room(4, [], none)),
    findall(room(ID, Clients, GS), room(ID, Clients, GS), Rooms),
    format('Rooms after setup: ~w~n', [Rooms]),

    find_available_room(room(ID, Clients, _)),
    format('Found room: ~w~n', [room(ID, Clients, _)]),
    assertion(ID = 1),
    assertion(Clients = [1,3,4]).