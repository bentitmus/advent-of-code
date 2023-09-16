% Advent of Code 2022 - Day 17
:- use_module(library(dcg/basics),     [eos//0, eol//0, integer//1, nonblanks//1]).
:- use_module(library(dcg/high_order), [sequence//2]).
:- use_module(library(pio),            [phrase_from_file/2]).

% DCG for input file to read list of moves
% Moves are represented by a cyclic list, which would fail an occurs check but
% is convenient here
directions(Directions) --> cyclic_directions(Directions-Directions).

cyclic_directions([repeat|Directions]-Directions) --> eol, eos.
cyclic_directions([left|Directions]-Tail)         --> "<", cyclic_directions(Directions-Tail).
cyclic_directions([right|Directions]-Tail)        --> ">", cyclic_directions(Directions-Tail).

% Convert a list of binary 1/0 values into a number
binary([], Answer, Answer).
binary([Value|Rest], Accumulator, Answer) :-
   NewAccumulator is (Accumulator << 1) + Value,
   binary(Rest, NewAccumulator, Answer).
binary(Digits, Binary) :- binary(Digits, 0, Binary).

% Standard shapes used in the chamber
% These are 9 wide including the walls, which helps to detect when the shape
% has been shifted too far
% The shapes/1 list is a cyclic list of shapes which would fail an occurs check
% but is convenient here
floor(Binary)  :- binary([1, 1, 1, 1, 1, 1, 1, 1, 1], Binary).
empty(Binary)  :- binary([1, 0, 0, 0, 0, 0, 0, 0, 1], Binary).
shape1(Binary) :- binary([0, 0, 0, 1, 1, 1, 1, 0, 0], Binary1),
                  Binary = [Binary1].
shape2(Binary) :- binary([0, 0, 0, 0, 1, 0, 0, 0, 0], Binary1),
                  binary([0, 0, 0, 1, 1, 1, 0, 0, 0], Binary2),
                  binary([0, 0, 0, 0, 1, 0, 0, 0, 0], Binary3),
                  Binary = [Binary1, Binary2, Binary3].
shape3(Binary) :- binary([0, 0, 0, 0, 0, 1, 0, 0, 0], Binary1),
                  binary([0, 0, 0, 0, 0, 1, 0, 0, 0], Binary2),
                  binary([0, 0, 0, 1, 1, 1, 0, 0, 0], Binary3),
                  Binary = [Binary1, Binary2, Binary3].
shape4(Binary) :- binary([0, 0, 0, 1, 0, 0, 0, 0, 0], Binary1),
                  binary([0, 0, 0, 1, 0, 0, 0, 0, 0], Binary2),
                  binary([0, 0, 0, 1, 0, 0, 0, 0, 0], Binary3),
                  binary([0, 0, 0, 1, 0, 0, 0, 0, 0], Binary4),
                  Binary = [Binary1, Binary2, Binary3, Binary4].
shape5(Binary) :- binary([0, 0, 0, 1, 1, 0, 0, 0, 0], Binary1),
                  binary([0, 0, 0, 1, 1, 0, 0, 0, 0], Binary2),
                  Binary = [Binary1, Binary2].
shapes(List)   :- shape1(Shape1), shape2(Shape2), shape3(Shape3),
                  shape4(Shape4), shape5(Shape5),
                  List = [Shape1, Shape2, Shape3, Shape4, Shape5|Tail],
                  Tail = List.

% Take an existing chamber and add enough space to the top to accomodate the new
% shape that will be added
prepare_chamber(Shape, Chamber, PreparedChamber) :-
   length(Shape, NumShapeRows),
   NumExtraRows is NumShapeRows + 3,
   length(ExtraRows, NumExtraRows),
   empty(Empty),
   maplist({Empty}/[Empty]>>true, ExtraRows),
   append(ExtraRows, Chamber, PreparedChamber).

% Remove blank rows from the top of the chamber
trim_chamber([EmptyRow|Rest], Result) :-
   empty(EmptyRow), !, trim_chamber(Rest, Result).
trim_chamber(Chamber, Chamber).

% Get the rows from the chamber that are needed for the shape
match_length(Shape, Chamber, MatchedChamber, RestChamber) :-
   length(Shape, NumRows),
   length(MatchedChamber, NumRows),
   append(MatchedChamber, RestChamber, Chamber).

% Etch the shape into the chamber rows
etch_line(ShapeLine, ChamberLine, EtchedLine) :- EtchedLine is ShapeLine \/ ChamberLine.
etch_shape(Shape, Chamber, EtchedChamber) :-
   match_length(Shape, Chamber, MatchedChamber, RestChamber),
   maplist(etch_line, Shape, MatchedChamber, EtchedLines),
   append(EtchedLines, RestChamber, EtchedChamber).

% Compute the moved shape line
move_line(left,  Line, Result) :- Result is Line << 1.
move_line(right, Line, Result) :- Result is Line >> 1.

% Check if there are any collisions for a line in the shape
is_clear(ShapeLine, ChamberLine) :- 0 is ShapeLine /\ ChamberLine.

% Check whether there are any collisions for a shape
check_move(Shape, Chamber) :-
   match_length(Shape, Chamber, ChamberRows, _),
   maplist(is_clear, Shape, ChamberRows).

% Attempt to move the shape if it can be moved, but leave it if it can't
try_move(Direction, Shape, Chamber, NewShape) :-
   maplist(move_line(Direction), Shape, NewShape),
   check_move(NewShape, Chamber), !.
try_move(_, Shape, _, Shape).

% Find the depths of the first block in each column which is needed to detect
% cycles
find_depth(Item, [Row|Rest], Acc, Result) :-
   0 is Item /\ Row,
   NewAcc is Acc + 1, !,
   find_depth(Item, Rest, NewAcc, Result).
find_depth(_, _, Acc, Acc).
find_depths(0, _, _) :- !.
find_depths(Index, Chamber, Depths) :-
   NewIndex is Index - 1,
   Item is 1 << NewIndex,
   find_depth(Item, Chamber, 0, Depth),
   nth0(NewIndex, Depths, Depth),
   find_depths(NewIndex, Chamber, Depths).

% Add points to detect cycles and then add a cycle to to the database to
% indicate when one has been found
% This could be passed along the stack, but it is easier to just add it to
% the database
:- dynamic point/3, cycle/2.

% Find a cycle by looking for points with the same identifier and computing the
% difference between these in the number of shapes and the depth of the chamber
find_cycle(_, _, _) :- cycle(_, _), !.
find_cycle(Point, ChamberDepth, NumShapes) :-
   point(Point, PrevStartLength, PrevNumShapes),
   PrevNumShapes \= NumShapes, !, 
   Dist is abs(PrevNumShapes - NumShapes),
   Length is abs(PrevStartLength - ChamberDepth),
   assertz(cycle(Length, Dist)),
   retractall(point(_, _, _)).
find_cycle(Point, ChamberDepth, NumShapes) :-
   assertz(point(Point, ChamberDepth, NumShapes)).

% Get the next move; if the next move is a 'repeat' it means we have hit the end
% of the move list and we should check for cycles
get_move(NumShapes, [repeat, Move|Moves], Shape, Chamber, Move, Moves) :- !,
   length(Chamber, ChamberDepth),
   length(Depths, 9),
   find_depths(9, Chamber, Depths),
   find_cycle((Shape, Depths), ChamberDepth, NumShapes).
get_move(_, [Move|Moves], _, _, Move, Moves).

% Run a single shape from top to bottom
% Prepare the chamber, run the shape etching it into the chamber when it rests,
% then trim the chamber
% When running, check for cycles
% Returns the 'etched' chamber and next move list
run_shape_(NumShapes, Shape, [ChamberRow|Chamber], StartMoves, [ChamberRow|FinalChamber], FinalMoves) :-
   get_move(NumShapes, StartMoves, Shape, [ChamberRow|Chamber], Move, Moves),
   try_move(Move, Shape, [ChamberRow|Chamber], NewShape),
   check_move(NewShape, Chamber), !,
   run_shape_(NumShapes, NewShape, Chamber, Moves, FinalChamber, FinalMoves).
run_shape_(NumShapes, Shape, Chamber, StartMoves, NewChamber, Moves) :-
   get_move(NumShapes, StartMoves, Shape, Chamber, Move, Moves),
   try_move(Move, Shape, Chamber, NewShape),
   etch_shape(NewShape, Chamber, NewChamber).
run_shape(NumShapes, Shape, Chamber, Moves, TrimmedChamber, FinalMoves) :-
   prepare_chamber(Shape, Chamber, PreparedChamber),
   run_shape_(NumShapes, Shape, PreparedChamber, Moves, FinalChamber, FinalMoves),
   trim_chamber(FinalChamber, TrimmedChamber).

% Determine the length of the chamber after running n shapes
% Cycles are detected and then subsequent runs skipped where possible
length_n_shapes(NumShapes, Shapes, Moves, Chamber, FinalChamber, AddLength, FinalAddLength) :-
   cycle(Length, PrevNumShapes), NumShapes > PrevNumShapes, !,
   retractall(cycle(_, _)),
   MultFactor is NumShapes // PrevNumShapes,
   NewAddLength is AddLength + (Length * MultFactor),
   RemainingShapes is NumShapes mod PrevNumShapes,
   length_n_shapes(RemainingShapes, Shapes, Moves, Chamber, FinalChamber, NewAddLength, FinalAddLength).
length_n_shapes(NumShapes, [Shape|Shapes], Moves, Chamber, FinalChamber, AddLength, FinalAddLength) :-
   NumShapes > 0, !,
   NewNumShapes is NumShapes - 1,
   run_shape(NumShapes, Shape, Chamber, Moves, NewChamber, NewMoves),
   length_n_shapes(NewNumShapes, Shapes, NewMoves, NewChamber, FinalChamber, AddLength, FinalAddLength).
length_n_shapes(0, _, _, Chamber, Chamber, AddLength, AddLength).
length_n_shapes(NumShapes, Moves, Length) :-
   floor(Floor),
   shapes(Shapes),
   length_n_shapes(NumShapes, Shapes, Moves, [Floor], FinalChamber, 0, AddLength),
   length(FinalChamber, LengthP1),
   Length is (LengthP1 - 1) + AddLength, !,
   retractall(point(_, _, _)).

:- phrase_from_file(directions(Moves), '2022/input/day17.txt'),
   length_n_shapes(2022, Moves, FirstLength),
   writeln(FirstLength),
   length_n_shapes(1000000000000, Moves, SecondLength),
   writeln(SecondLength).

:- begin_tests(aoc202217).

sample(Directions) :-
   Directions = [
      right, right, right, left,  left,  right, left,  right,
      right, left,  left,  left,  right, right, left,  right,
      right, right, left,  left,  left,  right, right, right,
      left,  left,  left,  right, left,  left,  left,  right,
      right, left,  right, right, left,  left,  right, right,
      repeat|Tail
   ],
   Tail = Directions.

test(binary) :-
   binary([1, 1], 3),
   binary([1, 0, 0, 0], 8).

test(shapes) :-
   shapes(Shapes),
   nth0(101, Shapes, [16, 56, 16]).

test(prepare_chamber) :-
   shape4(Shape),
   floor(Floor),
   prepare_chamber(Shape, [Floor], Chamber),
   length(Chamber, 8).

test(trim_chamber) :-
   shape4(Shape),
   floor(Floor),
   prepare_chamber(Shape, [Floor], Chamber),
   trim_chamber(Chamber, [Floor]).

test(is_clear) :-
   shape4([Shape|_]),
   empty(Empty),
   is_clear(Shape, Empty),
   \+ is_clear(Empty, Empty).

test(try_move) :-
   shape1(Shape),
   floor(Floor),
   prepare_chamber(Shape, [Floor], Chamber),
   try_move(right, Shape, Chamber, NewShape),
   try_move(right, NewShape, Chamber, UpdatedShape),
   UpdatedShape = NewShape.

test(etch_shape) :-
   shape5(Shape),
   empty(Empty),
   etch_shape(Shape, [Empty, Empty], [305, 305]).

test(run_shape) :-
   shape1(Shape),
   floor(Floor),
   sample(Moves),
   run_shape(1, Shape, [Floor], Moves, [317, Floor], [left|_]).

test(length_n_shapes) :-
   sample(Moves),
   length_n_shapes(10, Moves, 17).

test(sample_part_1) :-
   sample(Moves),
   length_n_shapes(2022, Moves, 3068).

test(sample_part_2) :-
   sample(Moves),
   length_n_shapes(1000000000000, Moves, 1514285714288).

:- end_tests(aoc202217).

:- run_tests(aoc202217).

:- halt.
