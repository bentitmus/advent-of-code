% Advent of Code 2022 - Day 1
:- use_module(library(dcg/basics)).
:- use_module(library(pio),   [phrase_from_file/2]).
:- use_module(library(lists), [sum_list/2, max_list/2]).
:- use_module(library(apply), [maplist/3]).

% DCG for input file to turn into a list of elves where each elf is a list
% of the items for that elf (as integers)
elves([])          --> eos, !.
elves([Elf|Elves]) --> elf(Elf), elves(Elves).

elf([])            --> ( eol; eos ), !.
elf([Item|Items])  --> integer(Item), eol, elf(Items).

% Map the sum over the list of elves; sort the results
highest_calories(CaloriesList) :-
   phrase_from_file(elves(ElvesList), '../input/01.txt'),
   maplist(sum_list, ElvesList, ElvesSums),
   sort(0, @>=, ElvesSums, CaloriesList).

:- highest_calories([Calories1, Calories2, Calories3|_]),
   write(Calories1),
   nl,
   Total is Calories1 + Calories2 + Calories3,
   write(Total),
   nl,
   halt.
