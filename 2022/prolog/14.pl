% Advent of Code 2022 - Day 14
:- use_module(library(dcg/basics),     [eos//0, eol//0, integer//1]).
:- use_module(library(dcg/high_order), [sequence//2]).
:- use_module(library(pio),            [phrase_from_file/2]).

% DCG for input file to a list of blocks, where each block is a sequence of
% points describing lines between the consecutive points
blocks(Blocks) --> sequence(block, Blocks), eos.

block([Point]) --> point(Point), eol, !.
block([Point|Points]) --> point(Point), " -> ", block(Points).

point((X, Y)) --> integer(X), ",", integer(Y).

% Converts a series of blocks to individual lines to make it easier to process
transform_to_lines([X, Y], [line(X, Y)]) :- !.
transform_to_lines([X, Y, Z|Rest], [line(X, Y)|RestT]) :-
   transform_to_lines([Y, Z|Rest], RestT).

transform_blocks_to_lines(Blocks, Lines) :-
   maplist(transform_to_lines, Blocks, LinesBeforeFlatten),
   flatten(LinesBeforeFlatten, Lines).

:- dynamic point/2, end/1.

% Create a new point
assert_point(X, Y) :- point(X, Y).
assert_point(X, Y) :- assertz(point(X, Y)).

% Create point/2 facts for each block in a line
add_line(line((X1, Y), (X2, Y))) :-
   X1 > X2, add_line(line((X2, Y), (X1, Y))), !.
add_line(line((X, Y1), (X, Y2))) :-
   Y1 > Y2, add_line(line((X, Y2), (X, Y1))), !.
add_line(line((X, Y), (X, Y))) :-
   assert_point(X, Y), !.
add_line(line((X1, Y), (X2, Y))) :-
   assert_point(X1, Y),
   NewX1 is X1 + 1,
   add_line(line((NewX1, Y), (X2, Y))), !.
add_line(line((X, Y1), (X, Y2))) :-
   assert_point(X, Y1),
   NewY1 is Y1 + 1,
   add_line(line((X, NewY1), (X, Y2))), !.

% Create point/2 facts for all points in all lines
make_points(Lines) :-
   maplist([Line, 1]>>add_line(Line), Lines, _).

% Calculate the last y value
last_y(Lines, LastY) :-
   maplist([line((_, Y1), (_, Y2)), Y]>>max_list([Y1, Y2], Y), Lines, Ys),
   max_list(Ys, LastY).

% Create an end/1 fact to stop adding sand when it is reached
make_end(Lines) :-
   last_y(Lines, LastY),
   assertz(end(LastY)).

% Create a plane (really a point/2 fact) after the last point for sand to rest
% against instead of continuing to fall
make_plane(Lines) :-
   last_y(Lines, LastY),
   TwoDeeper is LastY + 2,
   assertz(point(_, TwoDeeper)).

% Move the sand particle according to the rules, stopping when we reach the end
% or when the entry point is blocked
move_sand(X, 0) :-
   point(X, 0), !, fail.
move_sand(_, Y) :-
   end(Y), !, fail.
move_sand(X, Y) :-
   NewY is Y + 1,
   \+ point(X, NewY), !,
   move_sand(X, NewY).
move_sand(X, Y) :-
   NewY is Y + 1,
   NewX is X - 1,
   \+ point(NewX, NewY), !,
   move_sand(NewX, NewY).
move_sand(X, Y) :-
   NewY is Y + 1,
   NewX is X + 1,
   \+ point(NewX, NewY), !,
   move_sand(NewX, NewY).
move_sand(X, Y) :-
   assert_point(X, Y), !.

% Count the amount of sand particles added until no more can be or we reach the
% end (when move_sand/2 fails)
count_sand(Amount, FinalAmount) :-
   move_sand(500, 0),
   NewAmount is Amount + 1, !,
   count_sand(NewAmount, FinalAmount).
count_sand(Amount, Amount).
count_sand(Amount) :- count_sand(0, Amount).

% Remove all facts added by the point, end, and sand generation
cleanup :-
   retractall(point(_, _)),
   retractall(end(_)).

:- phrase_from_file(blocks(Blocks), '../input/14.txt'),
   transform_blocks_to_lines(Blocks, Lines),
   make_points(Lines),
   make_end(Lines),
   count_sand(AmountWithAbyss),
   writeln(AmountWithAbyss),
   cleanup,
   make_points(Lines),
   make_plane(Lines),
   count_sand(AmountWithPlane),
   writeln(AmountWithPlane),
   cleanup.

:- begin_tests(aoc202214).

sample(Blocks) :-
   Blocks = [
      [(498, 4), (498, 6), (496, 6)],
      [(503, 4), (502, 4), (502, 9), (494, 9)]
   ].

test(transform_to_lines) :-
   transform_blocks_to_lines(
      [[(1, 2), (3, 4), (5, 6)], [(7, 8), (9, 10)]],
      [line((1, 2), (3, 4)), line((3, 4), (5, 6)), line((7, 8), (9, 10))]
   ).

test(sample_adds_points) :-
   sample(Blocks),
   transform_blocks_to_lines(Blocks, Lines),
   make_points(Lines),
   findall((X, Y), point(X, Y), Points),
   length(Points, 20),
   cleanup.

test(sample_add_sand) :-
   sample(Blocks),
   transform_blocks_to_lines(Blocks, Lines),
   make_points(Lines),
   make_end(Lines),
   count_sand(24),
   cleanup.

test(sample_add_sand_with_infinite_plane) :-
   sample(Blocks),
   transform_blocks_to_lines(Blocks, Lines),
   make_points(Lines),
   make_plane(Lines),
   count_sand(93),
   cleanup.

:- end_tests(aoc202214).

:- run_tests(aoc202214).

:- halt.
