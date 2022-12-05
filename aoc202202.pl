% Advent of Code 2022 - Day 2
:- use_module(library(dcg/basics)).
:- use_module(library(pio),   [phrase_from_file/2]).
:- use_module(library(lists), [sum_list/2]).
:- use_module(library(apply), [maplist/3]).

% DCG for input file to turn into a list of pairs of numbers for each character
matches([]) --> eos, !.
matches([Match|Matches]) --> match(Match), matches(Matches).

match((Opponent, Strategy)) --> nonblank(Opponent), " ", nonblank(Strategy), eol.

% Transform A, B, C and X, Y, Z into 0, 1, 2 for a single item
transform_score((OpponentChar, StrategyChar), (Opponent, Strategy)) :-
   Opponent is OpponentChar - 65,
   Strategy is StrategyChar - 88.

% True if the Opponent and Strategy would produce the given score for the match
match_score(Opponent, Opponent, 3) :- !.
match_score(Opponent, Strategy, 6) :- Strategy is (Opponent + 1) rem 3, !.
match_score(Opponent, Strategy, 0) :- Strategy is (Opponent + 2) rem 3, !.

% First case is where the Strategy represents what we should play
score_first((Opponent, Strategy), Score) :-
   match_score(Opponent, Strategy, MatchScore),
   Score is Strategy + MatchScore + 1.

strategy(0, 0).
strategy(1, 3).
strategy(2, 6).

% Second case is where the Strategy tells us how many points to get from the match
% where strategy/2 is used to map to match points
score_second((Opponent, Strategy), Score) :-
   strategy(Strategy, MatchScore),
   match_score(Opponent, Choice, MatchScore), 
   Score is Choice + MatchScore + 1.

% Determine the score if a given algorithm is used for each match
total_score(AlgorithmForMatch, Score) :-
   phrase_from_file(matches(MatchesChars), 'aoc202202.txt'),
   maplist(transform_score, MatchesChars, Matches),
   maplist(AlgorithmForMatch, Matches, MatchesScores),
   sum_list(MatchesScores, Score).

:- total_score(score_first, ScoreFirst),
   write(ScoreFirst), nl,
   total_score(score_second, ScoreSecond),
   write(ScoreSecond), nl,
   halt.
