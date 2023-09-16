% Advent of Code 2022 - Day 9
:- use_module(library(dcg/basics),     [eos//0, eol//0, whites//0, integer//1]).
:- use_module(library(dcg/high_order), [sequence//2, optional//2]).
:- use_module(library(pio),            [phrase_from_file/2]).
:- use_module(library(yall)).

% DCG for input file to return a list of moves where a repeated move is repeated in the list
create_move(Direction, Repeat, Move) :-
   length(Move, Repeat),
   maplist({Direction}/[Direction]>>true, Move).

moves(Moves)      --> optional(eol, {true}), sequence(single_move, MovesList), whites, eos, {flatten(MovesList, Moves)}, !.

prefix            --> whites, optional("|", {true}).

single_move(Move) --> prefix, direction(Direction), " ", integer(Repeat), {create_move(Direction, Repeat, Move)}, eol.

direction((1,0))  --> "R".
direction((-1,0)) --> "L".
direction((0,1))  --> "U".
direction((0,-1)) --> "D".

% The rope is a list where the head is at the head of the list and the tail is the last element
% Every element is an offset to the next element except the tail which is the offset from the
% start
% A move is made to the head and then the rest of the knots are moved by determining what the
% move is needed using follow_head/3, the chain is moved using follow_chain/2
% Making a move on the next knot will update this knot because the position is an offset

% Apply a move to a position
apply_move_to_position((X, Y), (XMove, YMove), (NewX, NewY)) :-
   NewX is X + XMove, NewY is Y + YMove.

% Determine what move is needed for a particular axis, where the diagonal (first parameter)
% is 1 if a diagonal move is needed and 0 otherwise
follow_direction(1, 2, 1, 1).
follow_direction(1, -2, -1, -1).
follow_direction(0, X, 0, X) :- X < 2, X > -2.
follow_direction(1, X, X, 0) :- X < 2, X > -2.

% Takes an X, Y offset and determines what move must be made on the next knot and what our value
% would be if this move was made on the next knot
follow_head((XOffset, YOffset), (XMove, YMove), (NewXOffset, NewYOffset)) :-
   follow_direction(Diagonal, XOffset, XMove, NewXOffset),
   follow_direction(Diagonal, YOffset, YMove, NewYOffset), !.

% Cause all knots to follow the head and update each element based on the moves
follow_chain([EndOfChain], [EndOfChain]) :- !.
follow_chain([Head, Tail|TailChain], [NewHead, NewTail|NewTailChain]) :-
   follow_head(Head, TailMove, NewHead),
   apply_move_to_position(Tail, TailMove, MovedTail),
   follow_chain([MovedTail|TailChain], [NewTail|NewTailChain]).

% Make a move to the head and follow through will all knots in the rope
move(Move, [Head|TailChain], Result) :-
   apply_move_to_position(Head, Move, UpdatedHead),
   follow_chain([UpdatedHead|TailChain], Result).

% Apply all the moves and keep track of all the positions seen
apply_moves(Moves, Start, ListOfPositions) :-
   scanl(move, Moves, Start, ListOfPositions).

% Determine the unique positions of the tail when all the moves are made to the head
count_unique_positions(Knots, Moves, Count) :-
   start(Knots, Start),
   apply_moves(Moves, Start, ListOfPositions),
   maplist(last, ListOfPositions, ListOfTails),
   sort(ListOfTails, SortedList),
   length(SortedList, Count).

% Create a start where all the knots overlap
start(Length, Chain) :-
   length(Chain, Length),
   maplist([(0, 0)]>>true, Chain).

:- phrase_from_file(moves(Moves), '2022/input/day09.txt'),
   count_unique_positions(2, Moves, CountWith2Knots),
   writeln(CountWith2Knots),
   count_unique_positions(10, Moves, CountWith10Knots),
   writeln(CountWith10Knots).

% Examples to test the main predicate
:- begin_tests(aoc202209).

:- use_module(library(quasi_quotations), [phrase_from_quasi_quotation/2]).
:- quasi_quotation_syntax(move_listing).
move_listing(Content, _Vars, _Dict, Listing) :-
   phrase_from_quasi_quotation(moves(Listing), Content).

test(move_right_when_overlapping) :-
   move((1, 0), [(0,0), (2,2)], [(1,0), (2,2)]).

test(move_left_to_overlap) :-
   move((-1, 0), [(1,0), (2,2)], [(0,0), (2,2)]).

test(move_right_when_diagonal) :-
   move((1, 0), [(1,1), (7,2)], [(1,0), (8,3)]).

test(apply_moves_single_move) :-
   start(2, Start),
   apply_moves([(1, 0)], Start, [Start, [(1,0), (0,0)]]).

test(apply_moves_diagonal_away) :-
   start(2, Start),
   apply_moves([(1,0), (0,1), (1,0)],
               Start,
               [Start, [(1,0), (0,0)], [(1,1), (0,0)], [(1,0), (1,1)]]).

test(count_moves) :-
   count_unique_positions(2, [(1,0), (0,1), (1,0)], 2).

test(interpret_moves) :-
   Moves = {|move_listing||
            |R 2
            |U 1
            |L 4
            |D 1
            |},
   Moves = [(1,0),(1,0),(0,1),(-1,0),(-1,0),(-1,0),(-1,0),(0,-1)].

test(part_1_sample) :-
   Moves = {|move_listing||
            |R 4
            |U 4
            |L 3
            |D 1
            |R 4
            |D 1
            |L 5
            |R 2
            |},
   count_unique_positions(2, Moves, 13).

test(part_2_sample_1) :-
   Moves = {|move_listing||
            |R 4
            |U 4
            |L 3
            |D 1
            |R 4
            |D 1
            |L 5
            |R 2
            |},
   count_unique_positions(10, Moves, 1).

test(part_2_sample_2) :-
   Moves = {|move_listing||
            |R 5
            |U 8
            |L 8
            |D 3
            |R 17
            |D 10
            |L 25
            |U 20
            |},
   count_unique_positions(10, Moves, 36).

:- end_tests(aoc202209).

:- run_tests(aoc202209).

:- halt.
