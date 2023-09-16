% Advent of Code 2022 - Day 22
:- use_module(library(dcg/basics),     [eos//0, eol//0, integer//1, blanks//0]).
:- use_module(library(pio),            [phrase_from_file/2]).

% Create a maze outline
make_maze(Maze0, Maze1, Maze2, Maze3, Maze4, Maze5, 50-Maze) :-
   Maze = [
      0-face((1,   51),  Maze0),
      1-face((1,   101), Maze1),
      2-face((51,  51),  Maze2),
      3-face((101, 1),   Maze3),
      4-face((101, 51),  Maze4),
      5-face((151, 1),   Maze5)
   ].

% Convert a line of the maze for a face to a number with bits set indicating blocks
sym_num(46, 0).
sym_num(35, 1).
to_num(_, [], Num, Num) :- !.
to_num(Index, [Item|Rest], NumSoFar, FinalNum) :-
   NextNum is NumSoFar + (Item << Index),
   NextIndex is Index + 1,
   to_num(NextIndex, Rest, NextNum, FinalNum).
convert_to_num(RawLine, Line) :-
   maplist(sym_num, RawLine, SymLine),
   to_num(0, SymLine, 0, Line).

% DCG for input file to read maze and directions
maze_directions(Maze, Directions) --> maze(Maze), directions(Directions), eos, !.

maze(Maze) --> double_maze(Maze0, Maze1), single_maze(Maze2), double_maze(Maze3, Maze4), single_maze(Maze5), eol, {make_maze(Maze0, Maze1, Maze2, Maze3, Maze4, Maze5, Maze)}.

line(Line) --> {length(RawLine, 50)}, RawLine, {convert_to_num(RawLine, Line)}.

double_maze(0, [], [])                           --> !.
double_maze(Index, [Line1|Maze1], [Line2|Maze2]) --> blanks, line(Line1), line(Line2), eol, {NewIndex is Index - 1}, double_maze(NewIndex, Maze1, Maze2).
double_maze(Maze1, Maze2)                        --> double_maze(50, Maze1, Maze2).

single_maze(0, [])              --> !.
single_maze(Index, [Line|Maze]) --> blanks, line(Line), eol, {NewIndex is Index - 1}, single_maze(NewIndex, Maze).
single_maze(Maze)               --> single_maze(50, Maze).

directions_([])                                   --> eol.
directions_([move(Moves, turn_right)|Directions]) --> "R", integer(Moves), directions_(Directions).
directions_([move(Moves, turn_left)|Directions])  --> "L", integer(Moves), directions_(Directions).
directions([move(Moves, straight)|Directions])    --> integer(Moves), directions_(Directions).

% Helpers for move computation
dir(right, 1).
dir(down, 1).
dir(left, -1).
dir(up, -1).
moving(right, Row, Column, Column, Row).
moving(left, Row, Column, Column, Row).
moving(down, Row, Column, Row, Column).
moving(up, Row, Column, Row, Column).
extract_face(face(_, FaceChange, _, _, _, _), up,    FaceChange).
extract_face(face(_, _, FaceChange, _, _, _), right, FaceChange).
extract_face(face(_, _, _, FaceChange, _, _), down,  FaceChange).
extract_face(face(_, _, _, _, FaceChange, _), left,  FaceChange).
same_type(right, left).
same_type(up, down).
same_type(left, right).
same_type(down, up).
flip(Dir, Dir) :- !.
flip(Dir1, Dir2) :-
   \+ same_type(Dir1, Dir2),
   dir(Dir1, I1), dir(Dir2, I2),
   0 is I1 + I2, !.

% Check we can move within a face and then update the moved value
move_within(Length, Direction, Moving, Moved) :-
   dir(Direction, 1), Moved is Moving + 1, Moved < Length, !.
move_within(_, Direction, Moving, Moved) :-
   dir(Direction, -1), Moved is Moving - 1, Moved >= 0, !.

% Determine the new moved value when moving to a new face
new_moved(_, Direction, 0)  :- dir(Direction, -1), !.
new_moved(Length, _, Other) :- Other is Length - 1.

% Determine the non-moved value when moving to a new face - we might need to flip it
new_other(Length, Direction, NewDirection, Other, NewOther) :-
   flip(Direction, NewDirection), NewOther is (Length - 1) - Other, !.
new_other(_, _, _, Other, Other).

% Move a single step in a given direction, returning the new position and direction
% Two cases: (1) moving within a face, (2) moving to a new face
move(Length-_, pos(Face, Row, Column), Direction, pos(Face, NewRow, NewColumn), Direction) :-
   moving(Direction, Row, Column, Moving, Other),
   move_within(Length, Direction, Moving, Moved),
   moving(Direction, NewRow, NewColumn, Moved, Other), !.

move(Length-Maze, pos(Face, Row, Column), Direction, pos(NewFace, NewRow, NewColumn), NextDirection) :-
   moving(Direction, Row, Column, _Moving, Other),
   member(Face-FaceDesc, Maze), extract_face(FaceDesc, Direction, (NewFace, NewDirection)),
   new_moved(Length, NewDirection, Moved),
   new_other(Length, Direction, NewDirection, Other, NewOther),
   % This could have been better written by using the new direction rather than the opposite in the
   % connections list
   same_type(NewDirection, NextDirection),
   moving(NewDirection, NewRow, NewColumn, Moved, NewOther), !.

% Check whether a given position is free
check_position(_-Maze, pos(Face, Row, Column)) :-
   member(Face-face(_, _, _, _, _, Contents), Maze), !,
   nth0(Row, Contents, RowContents),
   0 is getbit(RowContents, Column).

% Change direction clockwise
change_direction(right, down).
change_direction(down,  left).
change_direction(left,  up).
change_direction(up,    right).

% Turn to a new direction
turn(Direction, straight, Direction)      :- !.
turn(Direction, turn_right, NewDirection) :- change_direction(Direction, NewDirection), !.
turn(Direction, turn_left, NewDirection)  :- change_direction(NewDirection, Direction).

% Change direction then make a move giving the new direction and position
nmove_(Maze, Number, (Position, Direction), FinalPositionDirection) :-
   Number > 0,
   move(Maze, Position, Direction, NextPosition, NextDirection),
   check_position(Maze, NextPosition), !,
   NewNumber is Number - 1,
   nmove_(Maze, NewNumber, (NextPosition, NextDirection), FinalPositionDirection).
nmove_(_, _, PositionDirection, PositionDirection).
nmove(Maze, move(Number, Turn), (Position, Direction), FinalPositionDirection) :-
   turn(Direction, Turn, NewDirection),
   nmove_(Maze, Number, (Position, NewDirection), FinalPositionDirection).

% Numbers used in the password calculation
facing_num(right, 0).
facing_num(down,  1).
facing_num(left,  2).
facing_num(up,    3).

% Compute the password for a given position and direction
password(_-Maze, pos(Face, Row, Column), Facing, Password) :-
   member(Face-face((StartRow, StartColumn), _, _, _, _, _), Maze), !,
   facing_num(Facing, FacingNum),
   FinalRow is StartRow + Row,
   FinalColumn is StartColumn + Column,
   Password is (FinalRow * 1000) + (FinalColumn * 4) + FacingNum.

% Run the maze and then compute the password
compute_password(Maze, Directions, Password) :-
   foldl(nmove(Maze), Directions, (pos(0, 0, 0), right), (FinalPosition, Facing)),
   password(Maze, FinalPosition, Facing, Password).

% Add connections to the maze outline
add_connection(Num-face(Pos, Row), (T, R, B, L), Num-face(Pos, T, R, B, L, Row)).
add_connections(Num-Maze, Connections, Num-MazeWithConnections) :-
   maplist(add_connection, Maze, Connections, MazeWithConnections).

% Connections for the part 1 maze
part1_connections(Connections) :-
   Connections = [
      ((4, down), (1, left), (2, up), (1, right)),
      ((1, down), (0, left), (1, up), (0, right)),
      ((0, down), (2, left), (4, up), (2, right)),
      ((5, down), (4, left), (5, up), (4, right)),
      ((2, down), (3, left), (0, up), (3, right)),
      ((3, down), (5, left), (3, up), (5, right))
   ].

% Connections for the part 2 maze
part2_connections(Connections) :-
   Connections = [
      ((5, left), (1, left),  (2, up),    (3, left)),
      ((5, down), (4, right), (2, right), (0, right)),
      ((0, down), (1, down),  (4, up),    (3, up)),
      ((2, left), (4, left),  (5, up),    (0, left)),
      ((2, down), (1, right), (5, right), (3, right)),
      ((3, down), (4, down),  (1, up),    (0, up))
   ].

:- phrase_from_file(maze_directions(Maze, Directions), '2022/input/day22.txt'),
   part1_connections(Connections1),
   add_connections(Maze, Connections1, Maze1),
   compute_password(Maze1, Directions, Password1),
   writeln(Password1),
   part2_connections(Connections2),
   add_connections(Maze, Connections2, Maze2),
   compute_password(Maze2, Directions, Password2),
   writeln(Password2).

:- begin_tests(aoc202222).

%     0
% 1 2 3
%     4 5
sample(4-Maze, Directions) :-
   Maze = [
      0-face((1, 9),  [8, 2, 1, 0]),
      1-face((5, 1),  [8, 0, 4, 0]),
      2-face((5, 5),  [0, 0, 8, 0]),
      3-face((5, 9),  [8, 1, 0, 4]),
      4-face((9, 9),  [8, 0, 2, 0]),
      5-face((9, 13), [0, 2, 0, 4])
   ],
   Directions = [
      move(10, straight),  move(5, turn_right), move(5, turn_left), move(10, turn_right),
      move(4,  turn_left), move(5, turn_right), move(5, turn_left)
   ].

sample_part1(Maze1, Directions) :-
   Connections = [
      ((4, down), (0, left), (3, up), (0, right)),
      ((1, down), (2, left), (1, up), (3, right)),
      ((2, down), (3, left), (2, up), (1, right)),
      ((0, down), (1, left), (4, up), (2, right)),
      ((3, down), (5, left), (0, up), (5, right)),
      ((5, down), (4, left), (5, up), (4, right))
   ],
   sample(Maze, Directions),
   add_connections(Maze, Connections, Maze1).

sample_part2(Maze2, Directions) :-
   Connections = [
      ((1, up),    (5, right), (3, up),   (2, up)),
      ((0, up),    (2, left),  (4, down), (5, down)),
      ((0, left),  (3, left),  (4, left), (1, right)),
      ((0, down),  (5, up),    (3, up),   (2, right)),
      ((3, down),  (5, left),  (1, down), (2, down)),
      ((3, right), (0, right), (1, left), (4, right))
   ],
   sample(Maze, Directions),
   add_connections(Maze, Connections, Maze2).

test(password) :-
   sample_part1(Maze, _),
   password(Maze, pos(2, 1, 3), right, 6032).

test(move1) :-
   sample_part1(Maze, _),
   move(Maze, pos(0, 3, 2), down, pos(3, 0, 2), _),
   move(Maze, pos(0, 2, 3), right, pos(0, 2, 0), _),
   move(Maze, pos(2, 2, 0), left, pos(1, 2, 3), _).

test(check_position) :-
   sample_part1(Maze, _),
   check_position(Maze, pos(0, 0, 0)),
   \+ check_position(Maze, pos(0, 0, 3)),
   \+ check_position(Maze, pos(4, 2, 1)),
   check_position(Maze, pos(5, 0, 3)).

test(nmove) :-
   sample_part1(Maze, _),
   nmove(Maze, move(10, straight),   (pos(0, 0, 0), right), (pos(0, 0, 2), right)),
   nmove(Maze, move(5,  turn_right), (pos(0, 0, 2), right), (pos(3, 1, 2), down)),
   nmove(Maze, move(5,  turn_left),  (pos(3, 1, 2), down),  (pos(1, 1, 3), right)),
   nmove(Maze, move(10, turn_right), (pos(1, 1, 3), right), (pos(1, 3, 3), down)).

test(compute_password1) :-
   sample_part1(Maze, Directions),
   compute_password(Maze, Directions, 6032).

test(move2) :-
   sample_part2(Maze, _),
   move(Maze, pos(0, 2, 3), right, pos(5, 1, 3), _).

test(compute_password2) :-
   sample_part2(Maze, Directions),
   compute_password(Maze, Directions, 5031).

:- end_tests(aoc202222).

:- run_tests(aoc202222).

:- halt.
