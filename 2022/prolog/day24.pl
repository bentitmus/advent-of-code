% Advent of Code 2022 - Day 24
:- use_module(library(dcg/basics),     [eos//0, eol//0, nonblanks//1]).
:- use_module(library(pio),            [phrase_from_file/2]).

% DCG for input file to get a list of blizzards
blizzards(Flattened) --> nonblanks(_), eol, rows(0, Rows), {flatten(Rows, Flattened)}.
rows(RIndex, [Row|Rows]) --> "#", row(RIndex, 0, Row), {NextRIndex is RIndex + 1}, rows(NextRIndex, Rows).
rows(_, []) --> nonblanks(_), eol, eos.
row(_, _, []) --> "#", eol.
row(RIndex, CIndex, Items) --> ".", {NextCIndex is CIndex + 1}, row(RIndex, NextCIndex, Items).
row(RIndex, CIndex, [(RIndex, CIndex, left)|Items]) --> "<", {NextCIndex is CIndex + 1}, row(RIndex, NextCIndex, Items).
row(RIndex, CIndex, [(RIndex, CIndex, right)|Items]) --> ">", {NextCIndex is CIndex + 1}, row(RIndex, NextCIndex, Items).
row(RIndex, CIndex, [(RIndex, CIndex, up)|Items]) --> "^", {NextCIndex is CIndex + 1}, row(RIndex, NextCIndex, Items).
row(RIndex, CIndex, [(RIndex, CIndex, down)|Items]) --> "v", {NextCIndex is CIndex + 1}, row(RIndex, NextCIndex, Items).

% Step the blizzards forward one move
step_blizzard((_, Columns), (R, C, right), (R, NC, right)) :- NC is (C + 1) mod Columns, !.
step_blizzard((_, Columns), (R, C, left),  (R, NC, left))  :- NC is (C - 1) mod Columns, !.
step_blizzard((Rows, _),    (R, C, down),  (NR, C, down))  :- NR is (R + 1) mod Rows, !.
step_blizzard((Rows, _),    (R, C, up),    (NR, C, up))    :- NR is (R - 1) mod Rows.
step_blizzards(Dim-BlizzardsList, Dim-NextBlizzardsList) :-
   maplist(step_blizzard(Dim), BlizzardsList, NextBlizzardsList), !.

% Create a tree from the list of blizzards
blizzards_tree(BlizzardsList, BlizzardsTree) :-
   maplist([(R, C, _), (R, C)-[]]>>true, BlizzardsList, Blizzards),
   list_to_rbtree(Blizzards, BlizzardsTree), !.

% Compute the free space tree for all available spaces - it appears that there
% are far more free spaces than blizzard spaces, so it will be faster to do the
% conversion once here and make the lookup quicker for each turn
free_space((RowsM1, ColumnsM1)-BlizzardsTree, (Row, Column)) :-
   between(0, RowsM1, Row), between(0, ColumnsM1, Column),
   \+ rb_lookup((Row, Column), _, BlizzardsTree).
free_spaces((Rows, Columns)-BlizzardsList, FreeSpaces) :-
   RowsM1 is Rows - 1, ColumnsM1 is Columns - 1,
   blizzards_tree(BlizzardsList, BlizzardsTree),
   findall(Pos-0, free_space((RowsM1, ColumnsM1)-BlizzardsTree, Pos), FreeList),
   list_to_rbtree(FreeList, FreeSpacesTmp1),
   rb_insert(FreeSpacesTmp1, (-1, 0), 0, FreeSpacesTmp2),
   rb_insert(FreeSpacesTmp2, (Rows, ColumnsM1), 0, FreeSpaces), !.

% Allowed moves are moves that do not move us into a blizzard
move((R, C), (R,  C)).
move((R, C), (NR, C)) :- NR is R + 1.
move((R, C), (NR, C)) :- NR is R - 1.
move((R, C), (R, NC)) :- NC is C + 1.
move((R, C), (R, NC)) :- NC is C - 1.
possible_move((Row, Column), FreeList, (NextRow, NextColumn)) :-
   move((Row, Column), (NextRow, NextColumn)),
   rb_lookup((NextRow, NextColumn), _, FreeList).

% Basic algorithm is BFS moving the blizzard on and recomputing the free spaces
% every time the index changes
bfs_next(BlizzardsList, _, FinalState, FinalIndex, [FinalIndex-FinalState|_], FinalIndex, BlizzardsList) :- !.
bfs_next(BlizzardsList, FreeSpaces, FinalState, Index, [Index-Pos|ToSearch], FinalIndex, EndList) :-
   NextIndex is Index + 1,
   findall(NextIndex-NextPos, possible_move(Pos, FreeSpaces, NextPos), AddToSearch),
   append(ToSearch, AddToSearch, NextToSearch), !,
   % Remove duplicates
   sort(NextToSearch, NextToSearchSorted),
   bfs_next(BlizzardsList, FreeSpaces, FinalState, Index, NextToSearchSorted, FinalIndex, EndList).
bfs_next(BlizzardsList, _, FinalState, _, [NextIndex-Pos|ToSearch], FinalIndex, EndList) :-
   step_blizzards(BlizzardsList, NextBlizzardsList),
   free_spaces(NextBlizzardsList, FreeSpaces), !,
   bfs_next(NextBlizzardsList, FreeSpaces, FinalState, NextIndex, [NextIndex-Pos|ToSearch], FinalIndex, EndList).
bfs(BlizzardsList, StartPoint, EndPoint, Index, EndList) :-
   free_spaces(BlizzardsList, FreeSpaces),
   bfs_next(BlizzardsList, FreeSpaces, EndPoint, 0, [0-StartPoint], Index, EndList), !.

:- phrase_from_file(blizzards(BlizzardsList), '2022/input/day24.txt'),
   step_blizzards((35, 100)-BlizzardsList, NextBlizzardsList),
   bfs(NextBlizzardsList, (-1, 0), (35, 99), IndexTrip1, TmpTree1),
   writeln(IndexTrip1),
   bfs(TmpTree1, (35, 99), (-1, 0), IndexTrip2, TmpTree2),
   bfs(TmpTree2, (-1, 0), (35, 99), IndexTrip3, _),
   Total is IndexTrip1 + IndexTrip2 + IndexTrip3,
   writeln(Total).

:- begin_tests(aoc202224).

sample((4, 6)-BlizzardsList) :-
   BlizzardsList = [
     (0, 0, right), (0, 1, right), (0, 3, left),  (0, 4, up),   (0, 5, left),
     (1, 1, left),  (1, 4, left),  (1, 5, left),
     (2, 0, right), (2, 1, down),  (2, 3, right), (2, 4, left), (2, 5, right),
     (3, 0, left),  (3, 1, up),    (3, 2, down),  (3, 3, up),   (3, 4, up),    (3, 5, right)
   ].

test(free_spaces) :-
   sample(BlizzardsList),
   free_spaces(BlizzardsList, FreeSpacesTree),
   rb_keys(FreeSpacesTree, [(-1, 0), (0, 2), (1, 0), (1, 2), (1, 3), (2, 2), (4, 5)]).

test(step_blizzards) :-
   sample(BlizzardsList),
   step_blizzards(BlizzardsList,  BlizzardsList1),
   step_blizzards(BlizzardsList1, BlizzardsList2),
   step_blizzards(BlizzardsList2, BlizzardsList3),
   free_spaces(BlizzardsList3, FreeSpacesTree),
   rb_keys(FreeSpacesTree, [(-1,0),(0,5),(1,0),(1,3),(1,5),(2,4),(2,5),(3,0),(3,1),(3,4),(3,5),(4,5)]).

test(possible_move) :-
   sample(BlizzardsList),
   step_blizzards(BlizzardsList,  BlizzardsList1),
   free_spaces(BlizzardsList1, FreeSpaces1),
   findall(Space, possible_move((-1, 0), FreeSpaces1, Space), [(-1, 0), (0, 0)]),
   step_blizzards(BlizzardsList1, BlizzardsList2),
   free_spaces(BlizzardsList2, FreeSpaces2),
   findall(Space, possible_move((0, 0), FreeSpaces2, Space), [(0, 0), (1, 0), (-1, 0)]).

test(bfs) :-
   sample(BlizzardsList),
   step_blizzards(BlizzardsList, NextBlizzardsList),
   bfs(NextBlizzardsList, (-1, 0), (4, 5), 18, TmpTree1),
   bfs(TmpTree1, (4, 5), (-1, 0), 23, TmpTree2),
   bfs(TmpTree2, (-1, 0), (4, 5), 13, _).

:- end_tests(aoc202224).

:- run_tests(aoc202224).

:- halt.
