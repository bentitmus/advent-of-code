% Advent of Code 2022 - Day 12
:- use_module(library(dcg/basics),     [eos//0, eol//0, nonblank//1]).
:- use_module(library(dcg/high_order), [sequence//2]).
:- use_module(library(pio),            [phrase_from_file/2]).

% DCG for input file giving a list of heights as numbers from 0 but with start and end
% atoms for those positions
height_map([]) --> eos, !.
height_map([HeightRow|Rows])  --> height_row(HeightRow), height_map(Rows).

height_row(HeightRow)         --> sequence(height_item, HeightRow), eol, !.

height_item(start)            --> "S".
height_item(end)              --> "E".
height_item((Item, unknown))  --> nonblank(Code), {Item is Code - 0'a}.

% Height map is indexed by two co-ordinates where X is the row and Y is the column
% The values are a pair with the item value from 0 (a) to 25 (z) and a distance which
% starts as unknown

% All possible moves from a particular co-ordinate
% Checks that the values can't be negative, but doesn't check height
possible_move((_, MaxY), (X, Y), (X, NewY)) :- NewY is Y + 1, NewY < MaxY.
possible_move((MaxX, _), (X, Y), (NewX, Y)) :- NewX is X + 1, NewX < MaxX.
possible_move(_,         (X, Y), (NewX, Y)) :- X > 0, NewX is X - 1.
possible_move(_,         (X, Y), (X, NewY)) :- Y > 0, NewY is Y - 1.

% Get the height of the given co-ordinate
get_height(HeightMap, (X, Y), Height) :-
   nth0(X, HeightMap, HeightRow),
   nth0(Y, HeightRow, (Height, _)), !.

% Get the distance from the start to the given co-ordinate
get_distance(HeightMap, (X, Y), Distance) :-
   nth0(X, HeightMap, HeightRow),
   nth0(Y, HeightRow, (_, Distance)), !.

% Set the distance to the given co-ordinate
set_distance(HeightMap, (X, Y), Distance, UpdatedHeightMap) :-
   nth0(X, HeightMap, HeightRow, RestHeightRows),
   nth0(Y, HeightRow, (Height, _), RestHeightRow),
   nth0(Y, UpdatedHeightRow, (Height, Distance), RestHeightRow),
   nth0(X, UpdatedHeightMap, UpdatedHeightRow, RestHeightRows), !.

% Check that the height of the adjacent node makes it a valid movement
check_height(HeightPredicate, HeightMap, CurrentNode, AdjacentNode) :-
   get_height(HeightMap, AdjacentNode, AdjacentHeight),
   get_height(HeightMap, CurrentNode, CurrentHeight),
   call(HeightPredicate, CurrentHeight, AdjacentHeight).

% Simplified traversal of graph keeping a list of nodes that have a value
% All movements from this list are explored before moving onto the next node
% This is simpler than Dijkstra because if we have already visited a node then we have the
% lowest value already (all moves have cost 1)
% Traverses the entire map before completing, rather than completing on an end node as this
% is easier for part 2 and performant enough
% Returns a final height map including the distances
route(_, HeightMap, _, [], HeightMap) :- !.
route(HeightPredicate, HeightMap, MaxPos, [CurrentNode|Nodes], FinalHeightMap) :-
   possible_move(MaxPos, CurrentNode, AdjacentNode),
   check_height(HeightPredicate, HeightMap, CurrentNode, AdjacentNode),
   get_distance(HeightMap, AdjacentNode, unknown), !,
   get_distance(HeightMap, CurrentNode, CurrentNodeDistance),
   NodeDistance is CurrentNodeDistance + 1,
   set_distance(HeightMap, AdjacentNode, NodeDistance, UpdatedHeightMap),
   append(Nodes, [AdjacentNode], NewNodes),
   route(HeightPredicate, UpdatedHeightMap, MaxPos, [CurrentNode|NewNodes], FinalHeightMap).
route(HeightPredicate, HeightMap, MaxPos, [_|Nodes], FinalHeightMap) :-
   route(HeightPredicate, HeightMap, MaxPos, Nodes, FinalHeightMap).

% Finds the index of an item with a particular value and optionally replaces it
find_and_replace_item([Row|Rows], Index, Item, Replacement, (Index, Y), [NewRow|Rows]) :-
   nth0(Y, Row, Item, Rest),
   nth0(Y, NewRow, Replacement, Rest).
find_and_replace_item([Row|Rows], Index, Item, Replacement, Position, [Row|NewRows]) :-
   NewIndex is Index + 1,
   find_and_replace_item(Rows, NewIndex, Item, Replacement, Position, NewRows).
find_and_replace_item(Rows, Item, Replacement, Position, NewRows) :-
   find_and_replace_item(Rows, 0, Item, Replacement, Position, NewRows).

% Explores a route using a given height predicate to determine what moves are valid and
% returns a final height map including distances
explore_route(HeightPredicate, HeightMap, StartPos, ValueAtStart, FinalHeightMap) :-
   length(HeightMap, MaxX),
   HeightMap = [HeightRow|_],
   length(HeightRow, MaxY),
   find_and_replace_item(HeightMap, (ValueAtStart, unknown), (ValueAtStart, 0), StartPos, UpdatedHeightMap),
   route(HeightPredicate, UpdatedHeightMap, (MaxX, MaxY), [StartPos], FinalHeightMap).

% Check that the height is valid
check_height_normal(Height, NewHeight) :- NewHeight is Height + 1, !.
check_height_normal(Height, NewHeight) :- NewHeight =< Height.

% Explore a route and find the distance for a given node
min_route(HeightMap, StartPos, EndPos, RouteLength) :-
   explore_route(check_height_normal, HeightMap, StartPos, 0, FinalHeightMap),
   get_distance(FinalHeightMap, EndPos, RouteLength), !.

% Check that the height is valid
check_height_inverse(Height, NewHeight) :- NewHeight is Height - 1, !.
check_height_inverse(Height, NewHeight) :- NewHeight >= Height.

% Explores all routes to the end from any valid start point and returns the smallest
% Actually do it in reverse exploring from the end and then returning the values at the start points
min_all_routes(HeightMap, EndPos, MinRouteLength) :-
   findall(StartPos, find_and_replace_item(HeightMap, (0, unknown), (0, unknown), StartPos, _), StartPosList),
   explore_route(check_height_inverse, HeightMap, EndPos, 25, FinalHeightMap),
   maplist(get_distance(FinalHeightMap), StartPosList, RouteLengthList),
   sort(RouteLengthList, [MinRouteLength|_]), !.

% Find the start and end and replace them with generic points of the appropriate value
extract_positions(HeightMapWithPoints, HeightMap, StartPos, EndPos) :-
   find_and_replace_item(HeightMapWithPoints, start, (0, unknown), StartPos, UpdatedHeightMap),
   find_and_replace_item(UpdatedHeightMap, end, (25, unknown), EndPos, HeightMap).

:- phrase_from_file(height_map(HeightMapWithPoints), '../input/day12.txt'),
   extract_positions(HeightMapWithPoints, HeightMap, StartPos, EndPos),
   min_route(HeightMap, StartPos, EndPos, MinRouteFromStart),
   writeln(MinRouteFromStart),
   min_all_routes(HeightMap, EndPos, MinRouteFromAllPossible),
   writeln(MinRouteFromAllPossible).

:- begin_tests(aoc202212).

sample(HeightMap, StartPos, EndPos) :-
   HeightMap = [
      [( 0, unknown), ( 0, unknown), ( 1, unknown), (16, unknown), (15, unknown), (14, unknown), (13, unknown), (12, unknown)],
      [( 0, unknown), ( 1, unknown), ( 2, unknown), (17, unknown), (24, unknown), (23, unknown), (23, unknown), (11, unknown)],
      [( 0, unknown), ( 2, unknown), ( 2, unknown), (18, unknown), (25, unknown), (25, unknown), (23, unknown), (10, unknown)],
      [( 0, unknown), ( 2, unknown), ( 2, unknown), (19, unknown), (20, unknown), (21, unknown), (22, unknown), ( 9, unknown)],
      [( 0, unknown), ( 1, unknown), ( 3, unknown), ( 4, unknown), ( 5, unknown), ( 6, unknown), ( 7, unknown), ( 8, unknown)]
   ],
   StartPos = (0, 0),
   EndPos   = (2, 5).

test(sample_part_1) :-
   sample(HeightMap, StartPos, EndPos),
   min_route(HeightMap, StartPos, EndPos, 31).

test(sample_part_2) :-
   sample(HeightMap, _, EndPos),
   min_all_routes(HeightMap, EndPos, 29).

:- end_tests(aoc202212).

:- run_tests(aoc202212).

:- halt.
