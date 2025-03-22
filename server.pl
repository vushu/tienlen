:- use_module(library(http/http_server)).
:- use_module(library(http/websocket)).
:- use_module(tienlen).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(record)).

:- initialization(start, main).

% Define WebSocket handler
:- http_handler(root(.), http_upgrade_to_websocket(handle_ws, []), [spawn([])]).

% Start the WebSocket server on port 8000
start :- http_server([port(8080)]).
% hands      scoreBoard  cardsInPlay  discardedCards, attackerindex, scoreboard nextMove

% Handle incoming WebSocket messages
handle_ws(WebSocket) :-
    ws_receive(WebSocket, Message),
    format('Received: ~w~n', [Message.data]),  % Print received message
    
    % term_string(Term, Message.data),
    init_game_state_as_json(GameStateJson),
    ws_send(WebSocket, json(GameStateJson)), % Send confirmation
    % writeln(JSONObject),
    

    
    % with_output_to(string(JsonString), json_write_dict(current_output, GameState, [])),

    ws_send(WebSocket, text('You are connected to Tien Len server.')), % Send confirmation
    % ws_send(WebSocket, json(JSONObject)), % Send confirmation
    handle_ws(WebSocket).  % Keep listening for new messages

initialize_game(GameState) :- initialize_game_full_players(GameState).

:- json_object
    card(rank: (text;integer), suit: text).

:- json_object
    next_move(player, action: any).

:- json_object
    place(place: any).

:- json_object
    single(single: any).

:- json_object
    pair(pair: list).

:- json_object
    sequence(sequence: list).

:- json_object
    three_of_kind(three_of_kind: list).

:- json_object
    four_of_kind(four_of_kind: list).

:- json_object
    double_sequence(double_sequence: list).

% hands      playerstates  cardsInPlay  discardedCards, attackerindex, scoreboard nextMove
:- json_object
    game_state(hands: list, player_states: list, cards_in_play: any, discarded_cards: list, attacker: any, scoreboard: list, next_move: any).

% initialize_game(GameState),

init_game_state_as_json(GameStateJson) :- initialize_game(GameState), prolog_to_json(GameState, GameStateJson).

                            % hands      scoreBoard  cardsInPlay  discardedCards, attackerindex, scoreboard nextMove
say_hi(Message) :- 
    writeln('Connected To TienLen Server'),
    writeln(Message).
%--------------- UNIT TESTS -------------------
:- use_module(library(plunit)).

:- begin_tests(tienlen_server).

test(test_initialize_game) :- 
    init_game_state_as_json(GS).

test(placing_sequence_json) :-
GS = game_state([[card(3,clubs),card(7,spades),card(7,hearts),card(8,clubs),card(q,diamonds),card(k,clubs),card(a,clubs)],[],[card(5,clubs),card(6,clubs),card(7,clubs),card(10,clubs),card(j,clubs),card(j,hearts),card(q,spades),card(k,spades),card(a,diamonds),card(a,hearts)],[card(3,diamonds),card(4,diamonds),card(4,hearts),card(6,hearts),card(7,diamonds),card(8,hearts),card(9,clubs),card(10,hearts),card(j,diamonds),card(k,diamonds)]],[in_play,first,in_play,in_play],sequence([card(j,spades),card(q,hearts),card(k,hearts),card(a,spades)]),[single(card(3,spades)),single(card(3,hearts)),single(card(4,spades)),single(card(4,clubs)),single(card(10,spades)),single(card(10,diamonds)),single(card(q,clubs)),single(card(2,clubs)),single(card(2,diamonds)),three_of_kind([card(5,spades),card(5,diamonds),card(5,hearts)]),three_of_kind([card(9,spades),card(9,diamonds),card(9,hearts)]),pair([card(6,spades),card(6,diamonds)]),pair([card(8,spades),card(8,diamonds)]),pair([card(2,spades),card(2,hearts)])],1,[],next_move(2,place(sequence([card(j,hearts),card(q,spades),card(k,spades),card(a,diamonds)])))),
prolog_to_json(GS, GSJSON).


:- end_tests(tienlen_server).