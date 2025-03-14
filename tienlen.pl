:- module(tienlen, [card_score/2, start_game/3, start_game/4, start_game/5, initialize_game_full_players/1, initialize_game_three_players/1, initialize_game_two_players/1]).
:- use_module(cards).

rank_value(3, 3). rank_value(4, 4). rank_value(5, 5).
rank_value(6, 6). rank_value(7, 7). rank_value(8, 8).
rank_value(9, 9). rank_value(10, 10). rank_value(j, 11).
rank_value(q, 12). rank_value(k, 13). rank_value(a, 14).
rank_value(2, 15).

suit_value(hearts, 4) :- !.
suit_value(diamonds, 3) :- !.
suit_value(clubs, 2) :- !.
suit_value(spades, 1) :- !.
suit_value(S, _) :- format(atom(Text),'Unknown suit ~w', [S]), throw(Text).


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

initialize_game_full_players(GameState) :- start_game(P1, P2, P3, P4, (PlayerIndex, Card)),
                            % hands      scoreBoard  cardsInPlay  discardedCards  nextMove
    GameState = game_state([P1, P2, P3, P4], [], none, [], next_move(PlayerIndex, place(single(Card)))).

initialize_game_three_players(GameState) :- start_game(P1, P2, P3, (PlayerIndex, Card)),
    GameState = game_state([P1, P2, P3], [], none, [], next_move(PlayerIndex, place(single(Card)))).
    
initialize_game_two_players(GameState) :- start_game(P1, P2, (PlayerIndex, Card)),
    GameState = game_state([P1, P2], [], none, [], next_move(PlayerIndex, place(single(Card)))).

get_next_move(game_state(_, _, _, _,_,Move), Move).

simulate_pass_move(game_state(Hands, ScoreBoard, CardsInplay, DiscardedCards, AttackerIndex, next_move(PlayerIndex, make_move)), 
    game_state(Hands, ScoreBoard, CardsInplay, DiscardedCards, AttackerIndex, next_move(PlayerIndex, pass))).

simulate_placing_cards(Cards, game_state(Hands, ScoreBoard, CardsInplay, DiscardedCards, AttackerIndex ,next_move(PlayerIndex, make_move)), 
    game_state(Hands, ScoreBoard, CardsInplay, DiscardedCards, AttackerIndex, next_move(PlayerIndex, place(Cards)))).

remove_cards([], Hand, Hand).
remove_cards([H | T], Hand, Rest) :-
    select(H, Hand, Remaining), 
    remove_cards(T, Remaining, Rest).


update_hands(EndIndex, Hands, Hand, UpdatedHands) :-
    update_hands_(EndIndex, 0, Hands, Hand, UpdatedHands).

update_hands_(EndIndex, EndIndex, [_ | T], Hand , [Hand | T]).
update_hands_(EndIndex, CurrentIndex, [H | T], Hand, [H|UpdatedHands]) :-
    NextIndex is CurrentIndex + 1,
    update_hands_(EndIndex, NextIndex, T, Hand, UpdatedHands).

whos_next(NumberOfPlayers, PlayerIndex, NextPlayer) :- 
    Temp is PlayerIndex + 1,
    (Temp >= NumberOfPlayers -> NextPlayer = 0; NextPlayer = Temp).

player_beats_cards_in_play(PlayerIndex, Hands, CardsInplay, PlacedCards, UpdatedHands) :-
    beats(PlacedCards, CardsInplay),
    tienlen_hand(RawCards,PlacedCards), % Really smart in prolog, that using the predicate the other direction will unwrap the cards, which is soo cool :D
    nth0(PlayerIndex, Hands, PlayerHand),
    remove_cards(RawCards, PlayerHand, NewPlayerHand),
    update_hands(PlayerIndex, Hands, NewPlayerHand, UpdatedHands).

make_move_or_pass(PlayerIndex, Hands, CardsInplay, Action) :-
    nth0(PlayerIndex, Hands, Hand), (possible_to_beat_with_hand(Hand, CardsInplay) -> Action = make_move; Action = pass).

% Game logic
% If AttackerIndex is the same as PlayerIndex, then the player can make a new move.
% We clear the cards in play
interpret_tienlen(game_state(Hands, ScoreBoard, CardsInplay, DiscardedCards, AttackerIndex, next_move(PlayerIndex, pass)), GameState) :-
    length(Hands, NumberOfPlayers),
    whos_next(NumberOfPlayers, PlayerIndex, AttackerIndex), % 
    append(DiscardedCards, [CardsInplay], NewDiscardedCards),
    writeln('Clearing cards in play since you are the attacker'),
    GameState = game_state(Hands, ScoreBoard, none, NewDiscardedCards, none, next_move(AttackerIndex, make_move)),!.


interpret_tienlen(game_state(Hands, ScoreBoard, CardsInplay, DiscardedCards, _, next_move(PlayerIndex, pass)), GameState) :-
    length(Hands, NumberOfPlayers),
    whos_next(NumberOfPlayers, PlayerIndex, NextPlayerIndex),
    make_move_or_pass(NextPlayerIndex, Hands, CardsInplay, Action),

    GameState = game_state(Hands, ScoreBoard, CardsInplay, DiscardedCards, PlayerIndex, next_move(NextPlayerIndex, Action)),!.

interpret_tienlen(game_state(Hands, [], none, [], none, next_move(PlayerIndex, place(single(Card)))), GameState) :-
    nth0(PlayerIndex, Hands, Hand),
    remove_cards([Card], Hand, Rest),
    update_hands(PlayerIndex, Hands, Rest, UpdatedHands),
    length(Hands, NumberOfPlayers),
    whos_next(NumberOfPlayers, PlayerIndex, NextPlayerIndex),
    GameState = game_state(UpdatedHands,[], single(Card), [], PlayerIndex, next_move(NextPlayerIndex, make_move)),!.

interpret_tienlen(game_state(Hands, [], CardsInplay, DiscardedCards, _, next_move(PlayerIndex, place(Cards))), GameState) :-
    player_beats_cards_in_play(PlayerIndex, Hands, CardsInplay, Cards, UpdatedHands),
    length(Hands, NumberOfPlayers),
    whos_next(NumberOfPlayers, PlayerIndex, NextPlayerIndex),
    append(DiscardedCards, [CardsInplay], NewDiscardedCards),
    make_move_or_pass(NextPlayerIndex, Hands, Cards, Action),
    GameState = game_state(UpdatedHands,[], Cards, NewDiscardedCards, PlayerIndex, next_move(NextPlayerIndex, Action)),!.


tienlen_hand([C], single(C)). % any single card is OK if no cards in play.
tienlen_hand([card(R, S), card(R, S2)], pair([card(R, S), card(R, S2)])).
tienlen_hand([card(R, S), card(R, S2), card(R, S3)], three_of_kind([card(R,S), card(R, S2), card(S3)])).
tienlen_hand([card(R, S), card(R, S2), card(R, S3), card(R, S4)], four_of_kind([card(R,S), card(R, S2), card(R, S3), card(R, S4)])).
tienlen_hand(Cards, sequence(Cards)) :-
    is_card_sequence(Cards), !.

tienlen_hand(Cards, double_sequence(Cards)) :-
    is_card_double_sequence(Cards), !.

is_card_sequence([card(R, _), card(R2,_), card(R3, _) | T ]) :-
    next(R, R2), next(R2, R3), next_rank_is_greater(R3, T).

is_card_double_sequence([card(R, _), card(R, _), card(R2, _), card(R2,_), card(R3, _), card(R3, _) | T]) :-
    next(R, R2), next(R2, R3), next_rank_is_greater_double(R3, T).

next(3, 4).
next(4, 5).
next(5, 6).
next(6, 7).
next(7, 8).
next(8, 9).
next(9, 10).
next(10, j).
next(j, q).
next(q, k).
next(k, a).
% next(a, 2).

highest_card(C, C, _) :- throw('Cannot compare card with itself').

highest_card(C1, C2, R) :-
    card_score(C1, C1Score),
    card_score(C2, C2Score), (C1Score > C2Score ->  R = C1; R = C2).

highest_card_in_list([H | T], Found) :-
    highest_card_in_list_(T, H, Found).

highest_card_in_list_([], F, F).
highest_card_in_list_([H | T], CurrentHighest, Found) :-
    highest_card(H, CurrentHighest, NewHighest),
    highest_card_in_list_(T, NewHighest, Found).

beats(single(C1), single(C2)) :-
    highest_card(C1, C2, C1), !.

beats(pair(C1), pair(C2)) :-
    highest_card_in_list(C1, F1),
    highest_card_in_list(C2, F2),
    highest_card(F1, F2, F1),!.

beats(three_of_kind(C1), three_of_kind(C2)) :-
    highest_card_in_list(C1, F1),
    highest_card_in_list(C2, F2),
    highest_card(F1, F2, F1),!.

beats(four_of_kind(C1), four_of_kind(C2)) :-
    highest_card_in_list(C1, F1),
    highest_card_in_list(C2, F2),
    highest_card(F1, F2, F1),!.

beats(sequence(C1), sequence(C2)) :-
    length(C1, L),
    length(C2, L),
    highest_card_in_list(C1, F1),
    highest_card_in_list(C2, F2),
    highest_card(F1, F2, F1).

beats(double_sequence(C1), double_sequence(C2)) :-
    length(C1, L),
    length(C2, L),
    highest_card_in_list(C1, F1),
    highest_card_in_list(C2, F2),
    highest_card(F1, F2, F1).

% Bombs
beats(double_sequence(_), single(card(2, _))) :- !.

beats(four_of_kind(_), single(card(2, _))).

beats(double_sequence(N), pair([card(2, _), card(2,_)])) :-
    length(N, 8), !.

beats(double_sequence(N), three_of_kind([card(2, _), card(2,_), card(2, _)])) :-
    length(N, 10), !.

next_rank_is_greater(_,[]).
next_rank_is_greater(PreviousRank, [card(R, _) | Rest]) :-
    next(PreviousRank, R), next_rank_is_greater(R, Rest).

next_rank_is_greater_double(_,[]).
next_rank_is_greater_double(PreviousRank, [card(R, _), card(R, _) | Rest]) :-
    next(PreviousRank, R), next_rank_is_greater_double(R, Rest).

rank_order(R1, R2) :-
    (next(R1, R2); (next(R1, X), rank_order(X, R2))).

% Custom sorting predicate using score.
sort_by_score(List, Sorted) :-
    predsort(compare_card_scores, List, Sorted).

compare_card_scores(Order, card(R1, S1), card(R2, S2)) :-
    card_score(card(R1, S1), Score1),
    card_score(card(R2, S2), Score2),
    compare(Order, Score1, Score2).

find_tienlen_hands([], []).

find_tienlen_hands([card(R,S) | T], [single(card(R,S))| Rest]) :-
    find_tienlen_hands(T, Rest).

find_tienlen_hands([card(R,S), card(R, S2) | T], [pair([card(R,S), card(R, S2)])| Rest]) :-
    find_tienlen_hands(T, Rest).
 
find_tienlen_hands([card(R,S), card(R, S2), card(R, S3) | T], [three_of_kind([card(R,S), card(R, S2), card(R, S3)]) | Rest]) :-
    find_tienlen_hands(T, Rest).

find_tienlen_hands([card(R,S), card(R, S2), card(R, S3), card(R, S4) | T], [four_of_kind([card(R,S), card(R, S2), card(R, S3), card(R, S4)]) | Rest]) :-
    find_tienlen_hands(T, Rest).

find_tienlen_hands([card(R1, S1), card(R2,S2), card(R3, S3) | T ], [sequence([card(R1, S1), card(R2, S2), card(R3, S3) | MoreSeq]) | Rest]) :-
    next(R1, R2),
    next(R2, R3),
    extended_sequence(R3, T, MoreSeq, Remaining),
    find_tienlen_hands(Remaining, Rest).

% Double sequence
find_tienlen_hands([card(R1, S1), card(R1, S2), card(R2, S3), card(R2, S4), card(R3, S5), card(R3, S6) | T], [double_sequence([card(R1, S1), card(R1, S2), card(R2, S3), card(R2, S4), card(R3, S5), card(R3, S6) | MoreSeq]) | Rest]) :-
    next(R1, R2),
    next(R2, R3),
    extended_double_sequence(R3, T, MoreSeq, Remaining),
    find_tienlen_hands(Remaining, Rest).

extended_sequence(PreviousRank,[card(R1, S1) | T], [card(R1, S1) | MoreSequence], Remaining) :-
    next(PreviousRank, R1),
    extended_sequence(R1,T, MoreSequence, Remaining).

extended_sequence(_,Remaining, [], Remaining).

extended_double_sequence(PreviousRank,[card(R1, S1), card(R1, S2) | T], [card(R1, S1), card(R1, S2) | MoreSequence], Remaining) :-
    next(PreviousRank, R1),
    extended_double_sequence(R1,T, MoreSequence, Remaining).

extended_double_sequence(_,Remaining, [], Remaining).

get_pairs_from_hand(Cards, Result) :-
    get_pairs_from_hand_(Cards, R), 
    list_to_set(R, Result).

get_pairs_from_hand_([], []).
get_pairs_from_hand_([H | T], R) :-
    F = pair(_),
    findall(F, memberchk(F, H), Bag),
    append(Bag, Rest, R),
    get_pairs_from_hand_(T, Rest).

get_third_of_kind_from_hand(Cards, Result) :-
    get_third_of_kind_from_hand_(Cards, R), 
    list_to_set(R, Result).

get_third_of_kind_from_hand_([], []).
get_third_of_kind_from_hand_([H | T], R) :-
    F = three_of_kind(_),
    findall(F, memberchk(F, H), Bag),
    append(Bag, Rest, R),
    get_third_of_kind_from_hand_(T, Rest).

get_four_of_kind_from_hand(Cards, Result) :-
    get_four_of_kind_from_hand_(Cards, R), 
    list_to_set(R, Result).

get_four_of_kind_from_hand_([], []).
get_four_of_kind_from_hand_([H | T], R) :-
    F = four_of_kind(_),
    findall(F, memberchk(F, H), Bag),
    append(Bag, Rest, R),
    get_four_of_kind_from_hand_(T, Rest).

get_sequence_from_hand(Cards, Result) :-
    get_sequence_from_hand_(Cards, R), 
    list_to_set(R, Result).

get_sequence_from_hand_([], []).

get_sequence_from_hand_([H | T], R) :-
    F = sequence(_),
    findall(F, memberchk(F, H), Bag),
    append(Bag, Rest, R),
    get_sequence_from_hand_(T, Rest).

get_double_sequence_from_hand(Cards, Result) :-
    get_double_sequence_from_hand_(Cards, R), 
    list_to_set(R, Result).

get_double_sequence_from_hand_([], []).

get_double_sequence_from_hand_([H | T], R) :-
    F = double_sequence(_),
    findall(F, memberchk(F, H), Bag),
    append(Bag, Rest, R),
    get_double_sequence_from_hand_(T, Rest).

get_all_possible_hands(SortedHand, FoundHands) :-
    findall(FoundHand, find_tienlen_hands(SortedHand, FoundHand), FoundHands).

possible_to_beat_with_hand(CurrentPlayerHand, single(Card)) :-
    length(CurrentPlayerHand, NumberCardsOfPlayer),
    NumberCardsOfPlayer >= 1,
    highest_card_in_list(CurrentPlayerHand, HighestCard),
    highest_card(HighestCard, Card, HighestCard).

possible_to_beat_with_hand(CurrentPlayerHand, pair(Cards)) :-
    length(Cards, NumbersInPlay), length(CurrentPlayerHand, NumberCardsOfPlayer),
    NumberCardsOfPlayer >= NumbersInPlay,
    get_all_possible_hands(CurrentPlayerHand, FoundHands),
    get_pairs_from_hand(FoundHands, ListOfPairs),
    member(Hand, ListOfPairs), beats(Hand, pair(Cards)).

possible_to_beat_with_hand(CurrentPlayerHand, three_of_kind(Cards)) :-
    length(Cards, NumbersInPlay), length(CurrentPlayerHand, NumberCardsOfPlayer),
    NumberCardsOfPlayer >= NumbersInPlay,
    get_all_possible_hands(CurrentPlayerHand, FoundHands),
    get_third_of_kind_from_hand(FoundHands, ListOfThreeOfKind),
    member(Hand, ListOfThreeOfKind), beats(Hand, three_of_kind(Cards)), !.

possible_to_beat_with_hand(CurrentPlayerHand, four_of_kind(Cards)) :-
    length(Cards, NumbersInPlay), length(CurrentPlayerHand, NumberCardsOfPlayer),
    NumberCardsOfPlayer >= NumbersInPlay,
    get_all_possible_hands(CurrentPlayerHand, FoundHands),
    get_four_of_kind_from_hand(FoundHands, ListOfFourOfKind),
    member(Hand, ListOfFourOfKind), beats(Hand, four_of_kind(Cards)), !.

possible_to_beat_with_hand(CurrentPlayerHand, sequence(Cards)) :-
    length(Cards, NumbersInPlay), length(CurrentPlayerHand, NumberCardsOfPlayer),
    NumberCardsOfPlayer >= NumbersInPlay,
    get_all_possible_hands(CurrentPlayerHand, FoundHands),
    get_sequence_from_hand(FoundHands, ListOfSequences),
    % we could optimize by only checking for sequences of the same length as Cards
    member(Hand, ListOfSequences), beats(Hand, sequence(Cards)), !.

possible_to_beat_with_hand(CurrentPlayerHand, double_sequence(Cards)) :-
    length(Cards, NumbersInPlay), length(CurrentPlayerHand, NumberCardsOfPlayer),
    NumberCardsOfPlayer >= NumbersInPlay,
    get_all_possible_hands(CurrentPlayerHand, FoundHands),
    get_double_sequence_from_hand(FoundHands, ListOfDoubleSequences),
    % we could optimize by only checking for double sequences of the same length as Cards
    member(Hand, ListOfDoubleSequences), beats(Hand, double_sequence(Cards)), !.


% Handling bombs
possible_to_beat_with_hand(CurrentPlayerHand, single(card(2,_))) :-
    length(CurrentPlayerHand, NumberCardsOfPlayer),
    NumberCardsOfPlayer >= 1,
    get_all_possible_hands(CurrentPlayerHand, FoundHands),
    get_double_sequence_from_hand(FoundHands, ListOfDoubleSequences),
    length(ListOfDoubleSequences, L), L > 0.

% If player has a four of kind, then they can beat any single 2 card.
possible_to_beat_with_hand(CurrentPlayerHand, single(card(2,_))) :-
    length(CurrentPlayerHand, NumberCardsOfPlayer),
    NumberCardsOfPlayer >= 1,
    get_all_possible_hands(CurrentPlayerHand, FoundHands),
    get_four_of_kind_from_hand(FoundHands, ListOfFourOfKind),
    length(ListOfFourOfKind, L), L > 0.

possible_to_beat_with_hand(CurrentPlayerHand, pair([card(2,_), card(2,_)])) :-
    length(CurrentPlayerHand, NumberCardsOfPlayer),
    NumberCardsOfPlayer >= 8, % must minimum have 8 cards to beat a double sequence to beat a pair of 2s
    get_all_possible_hands(CurrentPlayerHand, FoundHands),
    get_double_sequence_from_hand(FoundHands, ListOfDoubleSequences),
    member(double_sequence(Cards), ListOfDoubleSequences), length(Cards, 8),!.

 possible_to_beat_with_hand(CurrentPlayerHand, three_of_kind([card(2, _), card(2, _), card(2, _)])) :-
    length(CurrentPlayerHand, NumberCardsOfPlayer),
    NumberCardsOfPlayer >= 10, % must minimum have 10 cards to beat a double sequence to beat a three of kind of 2s
    get_all_possible_hands(CurrentPlayerHand, FoundHands),
    get_double_sequence_from_hand(FoundHands, ListOfDoubleSequences),
    member(double_sequence(Cards), ListOfDoubleSequences), length(Cards, 10),!.

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
    assertion(NextMove = next_move(3, place(single(card(3, spades))))).

test(is_tienlen_hand_single_card) :- 
    tienlen_hand([card(4, spades)], single(card(4, spades))).

test(is_tienlen_hand_pair) :- 
    tienlen_hand([card(4, spades), card(4, clubs)], pair(_)).

test(is_tienlen_hand_three_of_kind) :- 
    tienlen_hand([card(4, spades), card(4, clubs), card(4, diamonds)], three_of_kind(_)).

test(is_tienlen_hand_four_of_kind) :- 
    tienlen_hand([card(a, spades), card(a, clubs), card(a, diamonds), card(a, hearts)], four_of_kind(_)).

test(is_tienlen_hand_sequence_of_three) :- 
    tienlen_hand([card(3, spades), card(4, clubs), card(5, diamonds)], sequence([_, _, _])).

test(is_tienlen_hand_sequence_of_four) :- 
    tienlen_hand([card(3, spades), card(4, clubs), card(5, diamonds), card(6, hearts)], sequence([_, _, _, _])).

test(is_tienlen_hand_sequence_double_of_3) :- 
    tienlen_hand([card(3, spades), card(3, clubs), card(4, diamonds), card(4, hearts), card(5, spades), card(5, hearts)], double_sequence(_)).

test(is_tienlen_hand_sequence_double_of_4) :- 
    tienlen_hand([card(3, spades), card(3, clubs), card(4, diamonds), card(4, hearts), card(5, spades), card(5, hearts), card(6, clubs), card(6, diamonds)], double_sequence(_)).

test(sort_hand_by_score) :-
    Hand = [card(2, diamonds),card(3,clubs), card(4, hearts), card(4, spades)],
    sort_by_score(Hand, SortedHand), 
    assertion(SortedHand = [card(3,clubs),card(4,spades),card(4,hearts),card(2,diamonds)]).

test(find_tienlen_hands_four_of_kind) :-
    Hand = [card(2, spades),card(2, clubs), card(2, diamonds), card(2, hearts)],
    sort_by_score(Hand, SortedHand), 
    get_all_possible_hands(SortedHand, FoundHands),
    length(FoundHands, L),
    assertion(L = 8).

test(get_possible_hands) :-
    get_hands_test(Hands), length(Hands, 4).

test(winning_hand_single_cards, [fail]) :-
    beats(single((card(3,spades))), single(card(3, diamonds))).

test(winning_hand_single_cards) :-
    beats(single((card(a,spades))), single(card(3, diamonds))).

test(winning_hand_pair_cards) :-
    beats(pair([card(a, spades), card(a, diamonds)]), pair([card(q, diamonds), card(q, clubs)])).

test(winning_hand_pair_cards, [fail]) :-
    beats(pair([card(a, spades), card(a, diamonds)]), pair([card(a, clubs), card(a, hearts)])).

test(winning_hand_three_of_kind_cards, [fail]) :-
    beats(three_of_kind([card(5, spades), card(5, diamonds), card(5, hearts)]), three_of_kind([card(6, clubs), card(6, diamonds), card(6, hearts)])).

test(winning_hand_three_of_kind_cards) :-
    beats(three_of_kind([card(7, spades), card(7, diamonds), card(7, hearts)]), three_of_kind([card(6, clubs), card(6, diamonds), card(6, hearts)])).

test(winning_hand_four_of_kind_cards) :-
    beats(four_of_kind([card(7, spades), card(7, diamonds), card(7, hearts), card(7, clubs)]), four_of_kind([card(6, clubs), card(6, diamonds), card(6, hearts), card(6, spades)])).

test(winning_hand_four_of_kind_cards, [fail]) :-
    beats(four_of_kind([card(6, spades), card(6, diamonds), card(6, hearts), card(6, clubs)]), four_of_kind([card(8, clubs), card(8, diamonds), card(8, hearts), card(8, spades)])).

test(winning_hand_sequence_cards, [fail]) :-
    beats(sequence([card(3, spades), card(4, diamonds), card(5, hearts), card(6, clubs)]), sequence([card(4, clubs), card(5, diamonds), card(6, hearts), card(7, spades)])).

test(winning_hand_sequence_cards) :-
    beats(sequence([card(5, spades), card(6, diamonds), card(7, hearts), card(8, clubs)]), sequence([card(4, clubs), card(5, diamonds), card(6, hearts), card(7, spades)])).

test(winning_hand_sequence_cards, [fail]) :-
    beats(double_sequence([card(5, spades), card(5, diamonds), card(6, hearts), card(6, clubs), card(7, spades), card(7, diamonds)]), double_sequence([card(8, clubs), card(8, diamonds), card(9, hearts), card(9, spades), card(10, spades), card(10, clubs)])).

test(winning_hand_sequence_cards) :-
    beats(double_sequence([card(8, clubs), card(8, diamonds), card(9, hearts), card(9, spades), card(10, spades), card(10, clubs)]),
        double_sequence([card(5, spades), card(5, diamonds), card(6, hearts), card(6, clubs), card(7, spades), card(7, diamonds)])).

test(double_sequence_beats_single_2) :-
    beats(double_sequence([card(5, spades), card(5, diamonds), card(6, hearts), card(6, clubs), card(7, spades), card(7, diamonds)]), single(card(2, hearts))).

test(double_sequence_beats_pair_2) :-
    beats(double_sequence([card(5, spades), card(5, diamonds), 
        card(6, hearts), card(6, clubs),
        card(7, spades), card(7, diamonds),
        card(8, clubs), card(8, spades)]), 
    pair([card(2, hearts), card(2, spades)])).

test(double_sequence_beats_three_of_kind_2) :-
    beats(double_sequence([card(5, spades), card(5, diamonds), 
        card(6, hearts), card(6, clubs),
        card(7, spades), card(7, diamonds),
        card(8, clubs), card(8, spades),
        card(9, clubs), card(9, spades)]), 
    three_of_kind([card(2, hearts), card(2, spades), card(2, diamonds)])).

test(does_hand_have_pair) :- 
    get_predefined_hands(P1, _, _, _),
    sort_by_score(P1, SortedHand),
    get_all_possible_hands(SortedHand, FoundHands), 
    Expected = [pair([card(7,spades),card(7,hearts)]),pair([card(5,diamonds),card(5,hearts)]),pair([card(5,spades),card(5,diamonds)]),pair([card(3,clubs),card(3,hearts)])],
    get_pairs_from_hand(FoundHands, ListOfPairs), assertion(ListOfPairs = Expected).

test(does_hand_have_three_of_kind) :-
    get_predefined_hands(P1, _, _, _),
    sort_by_score(P1, SortedHand),
    get_all_possible_hands(SortedHand, FoundHands),
    Expected = [three_of_kind([card(5,spades),card(5,diamonds),card(5,hearts)])],
    get_third_of_kind_from_hand(FoundHands, ListOfThreeOfKind), assertion(ListOfThreeOfKind = Expected).

test(does_hand_have_four_of_kind) :-
    get_four_of_kind_predefined_hand(Hand),
    sort_by_score(Hand, SortedHand),
    get_all_possible_hands(SortedHand, FoundHands), 
    Expected = [four_of_kind([card(2,spades),card(2,clubs),card(2,diamonds),card(2,hearts)])],
    get_four_of_kind_from_hand(FoundHands, ListOfFourOfKind), assertion(ListOfFourOfKind = Expected).

test(does_hand_have_sequence) :-
    get_predefined_hands(P1, _, _, _),
    sort_by_score(P1, SortedHand),
    get_all_possible_hands(SortedHand, FoundHands),
    Expected = [sequence([card(q,diamonds),card(k,clubs),card(a,clubs)])],
    get_sequence_from_hand(FoundHands, ListOfSequences), assertion(ListOfSequences = Expected).

test(does_hand_have_double_sequence) :-
    get_double_sequence_predefined_hand(Hand),
    sort_by_score(Hand, SortedHand),
    get_all_possible_hands(SortedHand, FoundHands),
    get_double_sequence_from_hand(FoundHands, ListOfDoubleSequences), length(ListOfDoubleSequences, L),
    assertion(L = 3).

test(possible_to_beat_with_hand_three_of_kind, [fail]) :-
    get_predefined_hands(P1, _, _, _), % has three of kind of 5 where highest card suit is hearts
    sort_by_score(P1, SortedHand),
    possible_to_beat_with_hand(SortedHand, three_of_kind([card(6,spades),card(6,diamonds),card(6,hearts)])).

test(possible_to_beat_with_hand_four_of_kind) :-
    get_four_of_kind_predefined_hand(Hand),
    sort_by_score(Hand, SortedHand),
    possible_to_beat_with_hand(SortedHand, four_of_kind([card(a,spades),card(a,clubs),card(a,diamonds),card(a,hearts)])).

test(possible_to_beat_with_hand_sequence) :-
    get_predefined_hands(P1, _, _, _),
    sort_by_score(P1, SortedHand),
    possible_to_beat_with_hand(SortedHand, sequence([card(q,diamonds),card(k,clubs),card(a,spades)])).

test(possible_to_beat_with_hand_sequence) :-
    possible_to_beat_with_hand([card(4, spades), card(5, spades), card(6, spades), card(7, spades), card(8, spades), card(9, clubs)], sequence([card(5,clubs),card(6,clubs),card(7,clubs), card(8, clubs)])).

test(possible_to_beat_with_hand_double_sequence) :-
    get_double_sequence_predefined_hand(Hand),
    sort_by_score(Hand, SortedHand),
    possible_to_beat_with_hand(SortedHand, double_sequence([card(9,spades),card(9,diamonds),card(10,hearts),card(10,clubs),card(j,spades),card(j,diamonds)])).

test(possible_to_beat_with_hand_single_2, [fail]) :-
    get_predefined_hands(P1, _, _, _), % Highest card is 2 of diamonds
    sort_by_score(P1, SortedHand),
    possible_to_beat_with_hand(SortedHand, single(card(2,hearts))).

test(possible_to_beat_with_hand_single_2, [nondet]) :-
    get_double_sequence_predefined_hand(P1), % Highest card is 2 of diamonds
    sort_by_score(P1, SortedHand),
    possible_to_beat_with_hand(SortedHand, single(card(2,hearts))).

test(possible_to_beat_with_hand_pair_2) :-
    get_double_sequence_predefined_hand(P1),
    length(P1, L),
    L >= 8,
    sort_by_score(P1, SortedHand),
    possible_to_beat_with_hand(SortedHand, pair([card(2,hearts), card(2,spades)])).

test(possible_to_beat_with_hand_three_of_kind_2) :-
    get_double_sequence_predefined_hand_ten(P1),
    length(P1, L),
    L >= 10,
    sort_by_score(P1, SortedHand),
    possible_to_beat_with_hand(SortedHand, three_of_kind([card(2,hearts), card(2,spades), card(2,diamonds)])).

test(interpret_game_first_move) :-
    get_predefined_game(GameState),
    interpret_tienlen(GameState, NewGameState), get_next_move(NewGameState, next_move(0, make_move)).

test(interpret_game_pass_move) :-
    get_predefined_game(GameState),
    interpret_tienlen(GameState, FirstGameState), get_next_move(FirstGameState, next_move(0, make_move)),
    simulate_pass_move(FirstGameState, PassedGameState), 
    interpret_tienlen(PassedGameState, SecondGameState),
    get_next_move(SecondGameState, next_move(1, make_move)).

test(interpret_game_placing_single_card) :-
    get_predefined_game(GameState),
    interpret_tienlen(GameState, FirstGameState), 
    get_next_move(FirstGameState, next_move(0, make_move)),
    simulate_placing_cards(single(card(3,hearts)), FirstGameState, SecondGameState),
    interpret_tienlen(SecondGameState, game_state([P1, _,_, P4], _, CardsInplay, DiscardedCards,_, NextMove)),
    assertion(CardsInplay = single(card(3, hearts))),
    assertion(DiscardedCards = [single(card(3, spades))]),
    assertion(NextMove = next_move(1, make_move)),
    length(P1, L), 
    length(P4, L).

test(interpret_game_must_pass) :-
    GameState = game_state([[card(3, spades), card(a, diamonds), card(5, clubs)], [card(4, spades), card(3, hearts)]], [], none, [], none, next_move(0, place(single(card(3, spades))))),
    interpret_tienlen(GameState, NewGameState), 
    assertion(NewGameState = game_state([[card(a, diamonds), card(5, clubs)], [card(4, spades), card(3, hearts)]], [], single(card(3, spades)), [], _, next_move(1, make_move))),
    simulate_placing_cards(single(card(3,hearts)), NewGameState, PlaceGS), get_next_move(PlaceGS, next_move(1, place(single(card(3, hearts))))),
    interpret_tienlen(PlaceGS, SecondGameState), assertion(SecondGameState = game_state([[card(a, diamonds), card(5, clubs)], [card(4, spades)]], [], single(card(3, hearts)), [single(card(3, spades))],_, next_move(0, make_move))),
    simulate_placing_cards(single(card(a, diamonds)), SecondGameState, PlaceGS2), get_next_move(PlaceGS2, next_move(0, place(single(card(a, diamonds))))), 
    interpret_tienlen(PlaceGS2, ThirdGameState), assertion(ThirdGameState = game_state([[card(5, clubs)], [card(4, spades)]], [], single(card(a, diamonds)), [single(card(3, spades)), single(card(3, hearts))],_, next_move(1, pass))).

test(interpret_clearing_cards_in_play) :-
    GS = game_state([[card(5, clubs)], [card(4, spades)]], [], single(card(a, diamonds)), [single(card(3, spades)), single(card(3, hearts))], 0, next_move(1, pass)),
    interpret_tienlen(GS, NewGS), assertion(NewGS = game_state([[card(5, clubs)], [card(4, spades)]], [], none, [single(card(3, spades)), single(card(3, hearts)), single(card(a, diamonds))], none, next_move(0, make_move))).

initialize_game_predefined_full_players(GameState) :- 
    get_predefined_hands(P1, P2, P3, P4),
    append_multiple([P1, P2, P3, P4], Cards),
    player_with_lowest_card(Cards, (Player, Card)),!,
    GameState = game_state([P1, P2, P3, P4], [0, 0, 0, 0], [], [], none, next_move(Player, place(single(Card)))).

get_predefined_hands(P1, P2, P3, P4) :-
    P1 = [card(q,diamonds),card(5,diamonds),card(a,clubs),card(3,hearts),card(3,clubs),card(10,diamonds),card(2,diamonds),card(8,clubs),card(5,spades),card(7,spades),card(5,hearts),card(7,hearts),card(k,clubs)],
    P2 = [card(q,clubs),card(4,spades),card(9,hearts),card(j,spades),card(k,hearts),card(q,hearts),card(9,spades),card(6,spades),card(6,diamonds),card(a,spades),card(2,hearts),card(9,diamonds),card(2,spades)],
    P3 = [card(k,spades),card(a,hearts),card(7,clubs),card(a,diamonds),card(j,hearts),card(8,spades),card(8,diamonds),card(j,clubs),card(5,clubs),card(q,spades),card(4,clubs),card(10,clubs),card(6,clubs)],
    P4 = [card(2,clubs),card(9,clubs),card(3,spades),card(3,diamonds),card(4,diamonds),card(7,diamonds),card(4,hearts),card(j,diamonds),card(10,spades),card(k,diamonds),card(6,hearts),card(8,hearts),card(10,hearts)].

get_four_of_kind_predefined_hand(Hand) :-
    Hand = [card(2,clubs),card(5,clubs),card(2,spades),card(2,diamonds),card(4,diamonds),card(7,diamonds),card(4,hearts),card(j,diamonds),card(10,spades),card(k,diamonds),card(6,hearts),card(8,hearts),card(2,hearts)].

get_double_sequence_predefined_hand(Hand) :-
    Hand = [card(9,clubs),card(9,spades),card(10,spades),card(10,clubs),card(j,clubs),card(j,spades),card(q,clubs),card(q,spades)].

get_double_sequence_predefined_hand_ten(Hand) :-
    Hand = [card(9,clubs),card(9,spades),card(10,spades),card(10,clubs),card(j,clubs),card(j,spades),card(q,clubs),card(q,spades), card(k, spades), card(k, hearts), card(a, diamonds), card(a, clubs)].

get_test_hand(Hand) :-
    Hand = [card(q,diamonds),card(5,diamonds),card(a,clubs),card(3,hearts),card(3,clubs),card(10,diamonds),card(2,diamonds),card(8,clubs),card(5,spades),card(7,spades),card(5,hearts),card(7,hearts),card(k,clubs)].

get_small_test_hand(Hand) :-
    Hand = [card(q,diamonds),card(k,clubs), card(a, hearts), card(a, diamonds)].

get_small_numbered_hand(Hand) :-
    Hand = [card(2, diamonds),card(3,clubs), card(4, hearts), card(4, spades)].

get_hands_test(FoundHands) :-
    Hand = [card(j,spades), card(q,diamonds), card(k, hearts), card(a, clubs)],
    sort_by_score(Hand, SortedHand), 
    findall(FoundHand, find_tienlen_hands(SortedHand, FoundHand), FoundHands).

get_predefined_game(GameState) :-
    get_predefined_hands(P1, P2, P3, P4),
    GameState = game_state([P1, P2, P3, P4], [], none, [], none, next_move(3, place(single(card(3, spades))))).


:- end_tests(tienlen).