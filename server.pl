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

:- json_object
    card(rank: (text;integer), suit: text).

:- json_object
    game_state(player_turn: integer, hands: list).

% Handle incoming WebSocket messages
handle_ws(WebSocket) :-
    ws_receive(WebSocket, Message),
    format('Received: ~w~n', [Message.data]),  % Print received message
    
    % term_string(Term, Message.data),
    init_game_state_as_json(GameStateJson),
    ws_send(WebSocket, json(GameStateJson)), % Send confirmation

    test_json2(JSONObject),
    ws_send(WebSocket, json(JSONObject)), % Send confirmation
    % writeln(JSONObject),
    

    
    % with_output_to(string(JsonString), json_write_dict(current_output, GameState, [])),

    ws_send(WebSocket, text('You are connected to Tien Len server.')), % Send confirmation
    % ws_send(WebSocket, json(JSONObject)), % Send confirmation
    handle_ws(WebSocket).  % Keep listening for new messages

initialize_game(GameState) :- start_game(P1, P2, P3, P4, StartingPlayer),
    GameState = game_state(StartingPlayer, [P1, P2, P3, P4]).


% initialize_game(GameState),
test_json(JSONObject) :- prolog_to_json(hands([card(3, spades), card(k, hearts)], [], [] ,[]), JSONObject).
test_json2(JSONObject) :- prolog_to_json(game_state(1, [[card(3, spades), card(k, hearts)], [], [] ,[]]), JSONObject).

init_game_state_as_json(GameStateJson) :- initialize_game(GameState), prolog_to_json(GameState, GameStateJson).

say_hi(Message) :- 
    writeln('Connected To TienLen Server'),
    writeln(Message).