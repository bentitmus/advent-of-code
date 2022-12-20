% Advent of Code 2022 - Day 20
:- use_module(library(dcg/basics),     [eos//0, eol//0, integer//1, nonblanks//1]).
:- use_module(library(dcg/high_order), [sequence//2]).
:- use_module(library(pio),            [phrase_from_file/2]).

% DCG for input file to get a list of pairs of the number and its index
numbers(List) --> item(0, List).
item(_, []) --> eos.
item(Index, [(Item, Index)|List]) --> integer(Item), eol, {NextIndex is Index + 1}, item(NextIndex, List).

% Perform a single mix going through the items in original index order
mix(Length, OrigIndex, List, FinalList) :-
   OrigIndex < Length,
   nth0(ItemIndex, List, (Item, OrigIndex), Rest), !,
   NewIndex is (ItemIndex + Item) mod (Length - 1),
   nth0(NewIndex, NewList, (Item, OrigIndex), Rest),
   NewOrigIndex is OrigIndex + 1,
   mix(Length, NewOrigIndex, NewList, FinalList).
mix(_, _, List, List).
mix(List, FinalList) :-
   length(List, Length),
   mix(Length, 0, List, FinalList).

% Calculates the sum of the numbers at offset 1000, 2000, and 3000 from the zero
find_coordinates_sum(List, Coords) :-
   length(List, Length),
   % There's only one 0 in the list, but nth0/3 won't know that so there's a
   % choicepoint to cut
   nth0(Index0, List, (0, _)), !,
   IndexCoord1 is (Index0 + 1000) mod Length,
   IndexCoord2 is (Index0 + 2000) mod Length,
   IndexCoord3 is (Index0 + 3000) mod Length,
   nth0(IndexCoord1, List, (Coord1, _)),
   nth0(IndexCoord2, List, (Coord2, _)),
   nth0(IndexCoord3, List, (Coord3, _)),
   Coords is Coord1 + Coord2 + Coord3.

% Combine mixing a number times and getting the coordinate sum
mix_wrap(_Tag, List, FinalList) :-
   mix(List, FinalList).

mix_and_get_coords(NumberOfTimesToMix, List, Coords) :-
   length(Tags, NumberOfTimesToMix),
   foldl(mix_wrap, Tags, List, FinalList),
   find_coordinates_sum(FinalList, Coords).

% Adjust the coordinates by the multiplicative factor
adjust_coord((Item, Index), (AdjustedItem, Index)) :-
   AdjustedItem is Item * 811589153.

adjust_coords(List, AdjustedList) :-
   maplist(adjust_coord, List, AdjustedList).

:- phrase_from_file(numbers(List), '../input/20.txt'),
   mix_and_get_coords(1, List, Sum),
   writeln(Sum),
   adjust_coords(List, AdjustedList),
   mix_and_get_coords(10, AdjustedList, AdjustedSum),
   writeln(AdjustedSum).

:- begin_tests(aoc202220).

sample(List) :-
   List = [(1, 0), (2, 1), (-3, 2), (3, 3), (-2, 4), (0, 5), (4, 6)].

test(sample_part1) :-
   sample(List),
   mix(List, FinalList),
   FinalList = [(-2, 4), (1, 0), (2, 1), (-3, 2), (4, 6), (0, 5), (3, 3)],
   find_coordinates_sum(FinalList, 3),
   mix_and_get_coords(1, List, 3).

test(sample_part2) :-
   sample(List),
   adjust_coords(List, AdjustedList),
   mix_and_get_coords(10, AdjustedList, 1623178306).

:- end_tests(aoc202220).

:- run_tests(aoc202220).

:- halt.
