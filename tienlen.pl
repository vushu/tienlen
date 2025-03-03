:- module(tienlen, [card_score/2, start_game/3, start_game/4, start_game/5, initialize_game_full_players/1, initialize_game_three_players/1, initialize_game_two_players/1]).
:- use_module(cards).

rank_value(3, 3). rank_value(4, 4). rank_value(5, 5).
rank_value(6, 6). rank_value(7, 7). rank_value(8, 8).
rank_value(9, 9). rank_value(10, 10). rank_value(j, 11).
rank_value(q, 12). rank_value(k, 13). rank_value(a, 14).
rank_value(2, 15).

suit_value(hearts, 4).
suit_value(diamonds, 3).
suit_value(clubs, 2).
suit_value(spades, 1).


card_score(card(Rank, Suit), Score) :-
    rank_value(Rank, RankValue),
    suit_value(Suit, SuitValue),
    Score is (RankValue * 10) + SuitValue.

find_lowest_scored_card([Card], Card). 
find_lowest_scored_card([Card1, Card2 | Rest], LowestCard) :-
    card_score(Card1, Score1),
    card_score(Card2, Score2),
    (Score1 =< Score2 -> find_lowest_scored_card([Card1 | Rest], LowestCard)
                       ; find_lowest_scored_card([Card2 | Rest], LowestCard)).


start_game(P1, P2, PlayerStart) :- 
    deal_cards_to_players(P1, P2, _ , _),
    append_multiple([P1, P2], Cards),
    player_with_lowest_card(Cards, PlayerStart).

start_game(P1, P2, P3, PlayerStart) :- 
    deal_cards_to_players(P1, P2, P3 , _),
    append_multiple([P1, P2, P3], Cards),
    player_with_lowest_card(Cards, PlayerStart).

start_game(P1, P2, P3, P4, PlayerStart) :- 
    deal_cards_to_players(P1, P2, P3 , P4),
    append_multiple([P1, P2, P3, P4], Cards),
    player_with_lowest_card(Cards, PlayerStart).


append_multiple([], []).
append_multiple([H, H2 | T], R3) :-
    append(H, H2, R), append_multiple(T, R2), append(R, R2, R3).

no_hands_has_been_played(Lists) :-
    maplist(is_full_hand, Lists).

is_full_hand(H) :- length(H, 13).


player_with_lowest_card(Cards, R) :-
    find_lowest_scored_card(Cards, C),
    nth0(Index, Cards, C),
    Player is (Index // 13),
    R = (Player, C).

% number_of_cards

initialize_game_full_players(GameState) :- start_game(P1, P2, P3, P4, (Player, Card)),
    GameState = game_state([P1, P2, P3, P4], [], [], [], next_move(Player, place([Card]))).

initialize_game_three_players(GameState) :- start_game(P1, P2, P3, (Player, Card)),
    GameState = game_state([P1, P2, P3], [], [], [], next_move(Player, place([Card]))).
    
initialize_game_two_players(GameState) :- start_game(P1, P2, (Player, Card)),
    GameState = game_state([P1, P2], [], [], [], next_move(Player, place([Card]))).

get_next_move(game_state(_, _, _, _,Move), Move).

is_move_valid(Move) :- true. % For now.

make_next_move(game_state(Hands, _, _, _, next_move(Move))) :-
    is_move_valid(Move), (Player, Cards) = Move. 
    % TODO extract the cards from the player hands

%--------------- UNIT TESTS -------------------
:- use_module(library(plunit)).

:- begin_tests(tienlen).

test(no_hands_has_been_played_true) :-
    deal_cards_to_players(P1, P2, P3, P4),! ,
    assertion(no_hands_has_been_played([P1, P2, P3, P4])).

test(no_hands_has_been_played_fail, [fail]) :-
    no_hands_has_been_played([[1,2,3,4], []]).

test(player_with_lowers_card) :-
    deal_cards_to_players(P1, P2, P3, P4),
    append_multiple([P1, P2, P3, P4], Cards),
    player_with_lowest_card(Cards, (P, C)),!,
    assertion(P >= 0), 
    assertion(P =< 3),
    assertion(C == card(3, spades)).

test(first_round) :-
    get_predefined_hands(P1, P2, P3, P4),
    append_multiple([P1, P2, P3, P4], Cards),
    player_with_lowest_card(Cards, (Player, Card)),!,
    assertion(Player == 3), % 0 indexed
    assertion(Card == card(3, spades)).

test(next_move_is_player_places_lowest_card) :-
    initialize_game_predefined_full_players(GameState),!, 
    get_next_move(GameState, NextMove),
    assertion(NextMove = next_move(3, place([card(3, spades)]))).

initialize_game_predefined_full_players(GameState) :- 
    get_predefined_hands(P1, P2, P3, P4),
    append_multiple([P1, P2, P3, P4], Cards),
    player_with_lowest_card(Cards, (Player, Card)),!,
    GameState = game_state([P1, P2, P3, P4], [0, 0, 0, 0], [], [], next_move(Player, place([Card]))).

get_predefined_hands(P1, P2, P3, P4) :-
    P1 = [card(q,diamonds),card(5,diamonds),card(a,clubs),card(3,hearts),card(3,clubs),card(10,diamonds),card(2,diamonds),card(8,clubs),card(5,spades),card(7,spades),card(5,hearts),card(7,hearts),card(k,clubs)],
    P2 = [card(q,clubs),card(4,spades),card(9,hearts),card(j,spades),card(k,hearts),card(q,hearts),card(9,spades),card(6,spades),card(6,diamonds),card(a,spades),card(2,hearts),card(9,diamonds),card(2,spades)],
    P3 = [card(k,spades),card(a,hearts),card(7,clubs),card(a,diamonds),card(j,hearts),card(8,spades),card(8,diamonds),card(j,clubs),card(5,clubs),card(q,spades),card(4,clubs),card(10,clubs),card(6,clubs)],
    P4 = [card(2,clubs),card(9,clubs),card(3,spades),card(3,diamonds),card(4,diamonds),card(7,diamonds),card(4,hearts),card(j,diamonds),card(10,spades),card(k,diamonds),card(6,hearts),card(8,hearts),card(10,hearts)].

:- end_tests(tienlen).

