% Advent of Code 2022 - Day 3
:- use_module(library(dcg/basics)).
:- use_module(library(pio),     [phrase_from_file/2]).
:- use_module(library(ordsets), [ord_intersection/2]).
:- use_module(library(lists),   [sum_list/2]).
:- use_module(library(apply),   [maplist/3]).

% DCG for input file to turn into a list of rucksacks where each rucksack is a
% list of code points for the letters in that rucksack
rucksacks([]) --> eos, !.
rucksacks([Rucksack|Rucksacks]) --> rucksack(Rucksack), rucksacks(Rucksacks).

rucksack([]) --> eol, !.
rucksack([Item|Items]) --> nonblank(Item), rucksack(Items).

% DCG where these are organised by groups of three elves
elfgroups([]) --> eos, !.
elfgroups([[Rucksack1, Rucksack2, Rucksack3]|Rucksacks]) -->
   rucksack(Rucksack1), rucksack(Rucksack2), rucksack(Rucksack3),
   elfgroups(Rucksacks).

% Splits a rucksack into two compartments of equal size
split_rucksack(Rucksack, FirstCompartment, SecondCompartment) :-
   length(Rucksack, RucksackLength),
   CompartmentLength is RucksackLength // 2,
   length(FirstCompartment, CompartmentLength),
   length(SecondCompartment, CompartmentLength),
   append(FirstCompartment, SecondCompartment, Rucksack).

% Find the item that is in all lists
find_duplicate(Groups, Duplicate) :-
   maplist(sort, Groups, SortedGroups),
   ord_intersection(SortedGroups, [Duplicate]).

% Find the item that is in both compartments of a given rucksack
find_rucksack_duplicate(Rucksack, Duplicate) :-
   split_rucksack(Rucksack, FirstCompartment, SecondCompartment),
   find_duplicate([FirstCompartment, SecondCompartment], Duplicate).

% Convert an item into its priority
priority(Letter, Priority) :-
   Letter >= 0'a, !,
   Priority is Letter - 0'a + 1.
priority(Letter, Priority) :-
   Priority is Letter - 0'A + 27.

% Calculate the priority sum for a given DCG grammar with a given duplicate detection predicate
priority_sum(Grammar, List, DuplicatePred, Sum) :-
   phrase_from_file(Grammar, '2022/input/day03.txt'),
   maplist(DuplicatePred, List, DuplicatesList),
   maplist(priority, DuplicatesList, PriorityList),
   sumlist(PriorityList, Sum).

:- priority_sum(rucksacks(RucksacksList), RucksacksList, find_rucksack_duplicate, RucksacksSum),
   write(RucksacksSum), nl,
   priority_sum(elfgroups(ElfgroupsList), ElfgroupsList, find_duplicate, ElfgroupsSum),
   write(ElfgroupsSum), nl,
   halt.
