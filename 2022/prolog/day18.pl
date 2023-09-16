% Advent of Code 2022 - Day 18
:- use_module(library(dcg/basics),     [eos//0, eol//0, integer//1, nonblanks//1]).
:- use_module(library(dcg/high_order), [sequence//2]).
:- use_module(library(pio),            [phrase_from_file/2]).

% DCG for input file to get list of cubes, each of which is an x, y, z co-ordinate
cubes(Cubes)     --> sequence(pcube, Cubes), eos.
pcube((X, Y, Z)) --> integer(X), ",", integer(Y), ",", integer(Z), eol.

:- dynamic cube/3.

% Add all cubes as facts to save having to carry them around and make it easier
% to search
add_cubes([]) :- !.
add_cubes([(X, Y, Z)|Cubes]) :-
   assertz(cube(X, Y, Z)),
   add_cubes(Cubes).

% Cleanup the cube/3 facts after we've finished with them
remove_cubes :- retractall(cube(_, _, _)).

% A noncube is a 'cube' of water instead of the droplet
% Add an extra boundary of 1 to the parameters of the input which are all between
% 0 and 19 in all dimensions, so go between -1 and 20 to be safe and allow water
% to flow around the droplet
noncube(X, Y, Z) :-
   \+ cube(X, Y, Z),
   X >= -1, X =< 20,
   Y >= -1, Y =< 20,
   Z >= -1, Z =< 20.

% Find adjacent cube of the given type, giving all cubes on backtracking
neighbour(Type, (X, Y, Z), (XN, Y, Z)) :- XN is X - 1, call(Type, XN, Y, Z).
neighbour(Type, (X, Y, Z), (XN, Y, Z)) :- XN is X + 1, call(Type, XN, Y, Z).
neighbour(Type, (X, Y, Z), (X, YN, Z)) :- YN is Y - 1, call(Type, X, YN, Z).
neighbour(Type, (X, Y, Z), (X, YN, Z)) :- YN is Y + 1, call(Type, X, YN, Z).
neighbour(Type, (X, Y, Z), (X, Y, ZN)) :- ZN is Z - 1, call(Type, X, Y, ZN).
neighbour(Type, (X, Y, Z), (X, Y, ZN)) :- ZN is Z + 1, call(Type, X, Y, ZN).

% Find all neighbours of a given type
neighbours(Type, (X, Y, Z), Neighbours) :-
   findall(Neighbour, neighbour(Type, (X, Y, Z), Neighbour), Neighbours).

% Count the number of sides without neighbours for the current cube of the given
% type
count_cube(Type, Cube, Count, Neighbours) :-
   neighbours(Type, Cube, Neighbours),
   length(Neighbours, NeighboursCount),
   Count is 6 - NeighboursCount.

% Check whether we still need to visit the given cube
need_to_visit(Visited, ToVisit, Cube, Cube) :-
   \+ member(Cube, Visited),
   \+ member(Cube, ToVisit).

% Do a BFS of the given cube types returning the total count of surfaces without
% neighbours
bfs(_, [], _, Count, Count) :- !.
bfs(Type, [Cube|ToVisit], Visited, Count, FinalCount) :-
   count_cube(Type, Cube, CubeCount, Neighbours),
   NewCount is Count + CubeCount,
   convlist(need_to_visit(Visited, ToVisit), Neighbours, ToVisitNeighbours),
   append(ToVisit, ToVisitNeighbours, NewToVisit),
   bfs(Type, NewToVisit, [Cube|Visited], NewCount, FinalCount).
bfs(Type, Cubes, FinalCount) :-
   bfs(Type, Cubes, [], 0, FinalCount).

% Count surfaces for all cubes, giving the "ToVisit" list as all cubes because
% we don't care whether it is a connected surface
count_all(Cubes, Count) :-
   add_cubes(Cubes),
   bfs(cube, Cubes, Count),
   remove_cubes.

% Count the surface sides
% Count from the perspective of the water, and then take away the total outer
% surface of the water (22*22*6)
count_surface(Cubes, Count) :-
   add_cubes(Cubes),
   bfs(noncube, [(-1, -1, -1)], WaterCount),
   Count is WaterCount - (22*22*6),
   remove_cubes.

:- phrase_from_file(cubes(Cubes), '2022/input/day18.txt'),
   count_all(Cubes, AllCount),
   writeln(AllCount),
   count_surface(Cubes, SurfaceCount),
   writeln(SurfaceCount).

:- begin_tests(aoc202218).

sample(Cubes) :-
   Cubes = [
      (2, 2, 2),
      (1, 2, 2),
      (3, 2, 2),
      (2, 1, 2),
      (2, 3, 2),
      (2, 2, 1),
      (2, 2, 3),
      (2, 2, 4),
      (2, 2, 6),
      (1, 2, 5),
      (3, 2, 5),
      (2, 1, 5),
      (2, 3, 5)
   ].

test(add_remove_cubes) :-
   sample(Cubes),
   add_cubes(Cubes),
   cube(2, 2, 3),
   remove_cubes, !,
   \+ cube(2, 2, 3).

test(neighbours) :-
   sample(Cubes),
   add_cubes(Cubes),
   neighbours(cube, (2, 2, 3), [(2, 2, 2), (2, 2, 4)]),
   remove_cubes.

test(count_cube) :-
   sample(Cubes),
   add_cubes(Cubes),
   count_cube(cube, (2, 2, 3), 4, _),
   remove_cubes.

test(count_all) :-
   sample(Cubes),
   count_all(Cubes, 64).

test(count_surface) :-
   sample(Cubes),
   count_surface(Cubes, 58).

:- end_tests(aoc202218).

:- run_tests(aoc202218).

:- halt.
