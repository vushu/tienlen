:- module(cards,[generate_deck/1, generate_shuffled_deck/1, deal_cards_to_players/4]).
:- use_module(library(random)).

rank(3). rank(4). rank(5). rank(6). rank(7). rank(8). rank(9).
rank(10). rank(j). rank(q). rank(k). rank(a). rank(2).
suit(hearts). suit(diamonds). suit(clubs). suit(spades).

card(Rank, Suit) :- rank(Rank), suit(Suit).

generate_deck(Deck) :- findall(card(R, S), card(R, S), Deck).

shuffle_deck([], []).

shuffle_deck(Deck, [Card | RestShuffled]) :-
    random_member(Card, Deck),
    select(Card, Deck, RemainingDeck),
    shuffle_deck(RemainingDeck, RestShuffled).

generate_shuffled_deck(ShuffledDeck) :-
    generate_deck(Deck),
    shuffle_deck(Deck, ShuffledDeck).

deal_cards([], [], [], [], []).  % Base case: when no cards are left

deal_cards([C1, C2, C3, C4 | RestDeck], [C1 | P1], [C2 | P2], [C3 | P3], [C4 | P4]) :-
    deal_cards(RestDeck, P1, P2, P3, P4).

deal_cards_to_players(P1, P2, P3, P4) :-
    generate_shuffled_deck(S), deal_cards(S, P1, P2, P3, P4).