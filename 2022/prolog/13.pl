% Advent of Code 2022 - Day 13
:- use_module(library(dcg/basics),     [eos//0, eol//0, blanks//0, integer//1]).
:- use_module(library(dcg/high_order), [sequence//2, optional//2]).
:- use_module(library(pio),            [phrase_from_file/2]).

% DCG for input file to get list of pairs
pairs(Pairs)            --> sequence(pair, Pairs), eos.

pair((First, Second))   --> optional(eol, {true}), item(First), eol, item(Second), eol.

all_items([])           --> eos, !.
all_items([Item|Items]) --> blanks, item(Item), eol, all_items(Items).

item([])                --> "[]".
item([First|Rest])      --> "[", item(First), optional(sequence(item_with_comma, Rest), []), "]".
item(Value)             --> integer(Value).
item_with_comma(Item)   --> ",", item(Item).

% Equality predicate that ignores number of nested lists
equal(X, X) :- !.
equal([X|RestX], [Y|RestY]) :- equal(X, Y), equal(RestX, RestY), !.
equal(X, Y) :- integer(X), \+ integer(Y), equal([X], Y), !.
equal(X, Y) :- \+ integer(X), integer(Y), equal(X, [Y]), !.

% Ordering predicate to determine whether the first item is less than the second
less_than([], [_|_]) :- !.
less_than([X|_], [Y|_]) :- integer(X), integer(Y), X < Y, !.
less_than([X|_], [Y|_]) :- \+ integer(X), \+ integer(Y), less_than(X, Y), !.
less_than([X|_], [Y|_]) :- \+ integer(X), integer(Y), less_than(X, [Y]), !.
less_than([X|_], [Y|_]) :- integer(X), \+ integer(Y), less_than([X], Y), !.
less_than([X|First], [Y|Second]) :- equal(X, Y), less_than(First, Second), !.

% Less than predicate to be used by predsort
less_than(<, X, Y) :- less_than(X, Y), !.
less_than(>, _, _).

% Add a 1-based index to each tuple in the list
add_index(_, [], []).
add_index(Index, [Item|Rest], [(Index, Item)|UpdatedRest]) :-
   NewIndex is Index + 1,
   add_index(NewIndex, Rest, UpdatedRest).
add_index(List, UpdatedList) :- add_index(1, List, UpdatedList), !.

% Sum the indices (starting from 1) of a list of pairs
sum_indices_where_first_less_than(Pairs, SumOfIndices) :-
   add_index(Pairs, PairsWithIndex),
   convlist([(Index, X, Y), Index]>>less_than(X, Y), PairsWithIndex, FirstLessThan),
   sum_list(FirstLessThan, SumOfIndices).

% Adds two markers, sorts the list, then multiplies the indices of the two markers
find_marker_index_sum(Items, MarkerIndexSum) :-
   append([[[2]], [[6]]], Items, ItemsWithMarkers),
   predsort(less_than, ItemsWithMarkers, SortedItems),
   nth1(Index2, SortedItems, [[2]]),
   nth1(Index6, SortedItems, [[6]]),
   MarkerIndexSum is Index2 * Index6, !.

:- phrase_from_file(pairs(Pairs), '../input/13.txt'),
   sum_indices_where_first_less_than(Pairs, SumOfIndices),
   writeln(SumOfIndices),
   phrase_from_file(all_items(Items), '../input/13.txt'),
   find_marker_index_sum(Items, MarkerIndexSum),
   writeln(MarkerIndexSum).

:- begin_tests(aoc202213).

test(compare_single_pair_integer_ordering) :-
   less_than([1, 2, 3], [1, 2, 4]),
   \+ less_than([1, 2, 4], [1, 2, 3]).

test(compare_single_pair_shorter_list) :-
   less_than([1, 2], [1, 2, 4]),
   \+ less_than([1, 2, 4], [1, 2]).

test(compare_single_pair_two_lists) :-
   less_than([[1, 2, 3], 4], [[1, 2, 4], 4]),
   \+ less_than([[1, 2, 4], 4], [[1, 2, 3], 4]).

test(compare_single_pair_list_and_integer) :-
   less_than([1], [[1], 4]),
   \+ less_than([[2, 3], 4], [1, 4]).

test(compare_single_pair_to_get_sum) :-
   sum_indices_where_first_less_than([([1, 2, 3], [1, 2, 4])], 1),
   sum_indices_where_first_less_than([([1, 2, 4], [1, 2, 3])], 0).

test(compare_three_pairs_to_get_sum) :-
   sum_indices_where_first_less_than([
      ([1, 2, 3], [1, 2, 4]),
      ([4], [[1, 2, 3]]),
      ([1], [4])
   ], 4).

test(sample_part_2) :-
   List = [
      [1,1,3,1,1],
      [1,1,5,1,1],
      [[1],[2,3,4]],
      [[1],4],
      [9],
      [[8,7,6]],
      [[4,4],4,4],
      [[4,4],4,4,4],
      [7,7,7,7],
      [7,7,7],
      [],
      [3],
      [[[]]],
      [[]],
      [1,[2,[3,[4,[5,6,7]]]],8,9],
      [1,[2,[3,[4,[5,6,0]]]],8,9]
   ],
   find_marker_index_sum(List, 140).

:- end_tests(aoc202213).

:- run_tests(aoc202213).

:- halt.
