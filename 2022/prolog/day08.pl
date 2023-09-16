% Advent of Code 2022 - Day 8
:- use_module(library(dcg/basics), [eos//0, eol//0, nonblank//1]).
:- use_module(library(pio),        [phrase_from_file/2]).
:- use_module(library(clpfd),      [transpose/2]).
:- use_module(library(yall)).

% DCG for input file to return a list of tree lines which are a list of height of trees
trees([])                     --> eos, !.
trees([TreeLine|RestOfTrees]) --> tree_line(TreeLine), trees(RestOfTrees).

tree_line([])                 --> eol, !.
tree_line([Tree|RestOfTrees]) --> nonblank(TreeCode), {Tree is TreeCode - 48}, tree_line(RestOfTrees).

% Recursively check that a given predicate gives a particular value for visibility to a given tree
% Checks from left to right in a single tree line, and takes an accumulator which is passed to the update
% predicate
check_visible(_, _, [], []) :- !.
check_visible(UpdatePredicate, Accumulator, [Tree|RestOfTrees], [(Height, NewVisibility)|UpdatedRestOfTrees]) :-
   Tree = (Height, _),
   call(UpdatePredicate, Tree, Accumulator, NewVisibility, NewAccumulator),
   check_visible(UpdatePredicate, NewAccumulator, RestOfTrees, UpdatedRestOfTrees).

% Checks the update predicate on all tree rows in a given forest, from left to right
check_all_visible(UpdatePredicate, UpdateAccumulator, TreeRows, UpdatedTreeRows) :-
   maplist(check_visible(UpdatePredicate, UpdateAccumulator), TreeRows, UpdatedTreeRows).

% Checks the update predicate on all tree rows in a given forest, from all angles
check_all_angles(UpdatePredicate, UpdateAccumulator, Trees, UpdatedTrees) :-
   check_all_visible(UpdatePredicate, UpdateAccumulator, Trees, TreesLeft),
   maplist(reverse, TreesLeft, TreesReversed),
   check_all_visible(UpdatePredicate, UpdateAccumulator, TreesReversed, TreesRight),
   transpose(TreesRight, TreesTransposed),
   check_all_visible(UpdatePredicate, UpdateAccumulator, TreesTransposed, TreesTop),
   maplist(reverse, TreesTop, TreesTransposedReversed),
   check_all_visible(UpdatePredicate, UpdateAccumulator, TreesTransposedReversed, UpdatedTrees).

% Applies the update predicate to all trees in the forest using check_visible/4 and then applies an
% accumulator predicate returning the single result
apply_scoring(Trees, DefaultVisibility, UpdatePredicate, UpdateAccumulator, AccumulationPredicate, Result) :-
   maplist(maplist({DefaultVisibility}/[Tree, (Tree, DefaultVisibility)]>>true), Trees, TreesWithVisibility), !,
   check_all_angles(UpdatePredicate, UpdateAccumulator, TreesWithVisibility, UpdatedTrees),
   flatten(UpdatedTrees, FlatTrees),
   maplist([(_, Visibility), Visibility]>>true, FlatTrees, Visibilities),
   call(AccumulationPredicate, Visibilities, Result).

% Update predicate to determine whether the tree is visible from the edge of the forest
% 'visibility' corresponds to boolean value of whether the tree is visible
update_visibility_los((Height, _), HighestTreeSoFar, 1, Height) :-
   Height > HighestTreeSoFar, !.
update_visibility_los((_, Visibility), HighestTreeSoFar, Visibility, HighestTreeSoFar).

% Counts all visible trees from all angles
count_all_visible(Trees, NumberVisible) :-
   apply_scoring(Trees, 0, update_visibility_los, -1, sum_list, NumberVisible).

% Updates the 'accumulator' for update_scenic_score/4 extracting the current score for the tree of the
% specified height
update_tree_scores(_, _, [], [], 0).
update_tree_scores(Height, Height, [HeightScore|RestOfScores], [1|UpdatedRestOfScores], HeightScore) :-
   NewIndex is Height + 1, update_tree_scores(Height, NewIndex, RestOfScores, UpdatedRestOfScores, _).
update_tree_scores(Height, Index, [_|RestOfScores], [1|UpdatedRestOfScores], HeightScore) :-
   Height > Index, NewIndex is Index + 1, update_tree_scores(Height, NewIndex, RestOfScores, UpdatedRestOfScores, HeightScore).
update_tree_scores(Height, Index, [TreeScore|RestOfScores], [NewTreeScore|UpdatedRestOfScores], _) :-
   NewIndex is Index + 1, NewTreeScore is TreeScore + 1, update_tree_scores(Height, NewIndex, RestOfScores, UpdatedRestOfScores, _).

% Update predicate to determine the scenic score for each tree in the forest
% 'visibility' is the scenic score so far (should default to 1)
% 'accumulator' is the list of distances from a tree of a given height to this tree
%   where the edge of the forest is used as 0 distance
update_scenic_score((Height, ScenicScore), TreeScores, NewScenicScore, NewTreeScores) :-
   update_tree_scores(Height, 0, TreeScores, NewTreeScores, HeightScore), !,
   NewScenicScore is ScenicScore * HeightScore.

% Predicate to check that all elements in list are the same
all_same(_, []) :- !.
all_same(H, [H|T]) :- all_same(H, T).

% Determines the greatest scenic score
max_scenic_score(Trees, MaxScore) :-
   length(TreeScores, 10),
   all_same(0, TreeScores),
   apply_scoring(Trees, 1, update_scenic_score, TreeScores, max_list, MaxScore).

:- phrase_from_file(trees(Trees), '2022/input/day08.txt'),
   count_all_visible(Trees, Count),
   writeln(Count),
   max_scenic_score(Trees, MaxScore),
   writeln(MaxScore).

% Examples to test the main predicate
:- begin_tests(aoc202208).

test(single_row_visibility_from_left) :-
   check_visible(update_visibility_los, -1, [(1,0), (3,0), (2,0), (1,0), (9,0)], [(1,1), (3,1), (2,0), (1,0), (9,1)]).

test(all_rows_visibility_from_left) :-
   check_all_visible(update_visibility_los, -1, [
      [(1,0), (3,0), (2,0), (1,0), (9,0)],
      [(9,0), (3,0), (2,0), (1,0), (9,0)]
   ], [
      [(1,1), (3,1), (2,0), (1,0), (9,1)],
      [(9,1), (3,0), (2,0), (1,0), (9,0)]
   ]).

test(visibility_from_all_angles) :-
   check_all_angles(update_visibility_los, -1, [
      [(1,0), (3,0), (2,0)],
      [(9,0), (3,0), (4,0)],
      [(7,0), (5,0), (2,0)]
   ], [
      [(2,1), (4,1), (2,1)],
      [(5,1), (3,0), (3,1)],
      [(7,1), (9,1), (1,1)]
   ]).

test(count_visibility) :-
   count_all_visible([
      [3,0,3,7,3],
      [2,5,5,1,2],
      [6,5,3,3,2],
      [3,3,5,4,9],
      [3,5,3,9,0]
   ], 21).

test(scenic_score) :-
   max_scenic_score([
      [3,0,3,7,3],
      [2,5,5,1,2],
      [6,5,3,3,2],
      [3,3,5,4,9],
      [3,5,3,9,0]
   ], 8).

:- end_tests(aoc202208).

:- run_tests(aoc202208).

:- halt.
