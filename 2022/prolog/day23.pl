% Advent of Code 2022 - Day 23
:- use_module(library(dcg/basics),     [eos//0, eol//0, integer//1, blanks//0]).
:- use_module(library(pio),            [phrase_from_file/2]).

% This approach is slow and takes around 30 minutes for part 2
% Another Prolog solution takes around a minute, so this can be improved
% Originally this used red-black trees but I moved to using sets and it made
% little difference except that it could be parallelised (which does make a
% difference); since the other Prolog solution uses red-black trees and a similar
% approach, I suspect that the inner loop here is very inefficient

% DCG for input file to get a list of elf positions
elves(Set) --> elves(0, Elves), {flatten(Elves, List), list_to_ord_set(List, Set)}, !.
elves(_, []) --> eos, !.
elves(Row, [Line|Lines]) --> line(Row, 0, Line), !, {NewRow is Row + 1}, elves(NewRow, Lines).
line(_, _, []) --> eol, !.
line(Row, Column, Line) --> ".", {NewColumn is Column + 1}, line(Row, NewColumn, Line).
line(Row, Column, [(Row, Column)|Line]) --> "#", {NewColumn is Column + 1}, line(Row, NewColumn, Line).

% Determine the neighbours of a given position and their direction
neighbour((R, C), nw-(NR, NC)) :- NR is R - 1, NC is C - 1.
neighbour((R, C),  n-(NR, C))  :- NR is R - 1.
neighbour((R, C), ne-(NR, NC)) :- NR is R - 1, NC is C + 1.
neighbour((R, C),  e-(R,  NC)) :- NC is C + 1.
neighbour((R, C), se-(NR, NC)) :- NR is R + 1, NC is C + 1.
neighbour((R, C),  s-(NR, C))  :- NR is R + 1.
neighbour((R, C), sw-(NR, NC)) :- NR is R + 1, NC is C - 1.
neighbour((R, C),  w-(R,  NC)) :- NC is C - 1.
neighbour(Positions, Position, NDir) :- neighbour(Position, NDir-Neighbour), ord_memberchk(Neighbour, Positions).

% Find all neighbours for a given position
neighbours(Positions, Position, Neighbours) :-
   findall(Neighbour, neighbour(Positions, Position, Neighbour), UnsortedNeighbours),
   list_to_ord_set(UnsortedNeighbours, Neighbours).

% Determine whether it is possible to move in a particular direction
can_move(Neighbours, n) :-
   \+ ord_memberchk(nw, Neighbours), \+ ord_memberchk(n, Neighbours), \+ ord_memberchk(ne, Neighbours).
can_move(Neighbours, s) :-
   \+ ord_memberchk(sw, Neighbours), \+ ord_memberchk(s, Neighbours), \+ ord_memberchk(se, Neighbours).
can_move(Neighbours, w) :-
   \+ ord_memberchk(nw, Neighbours), \+ ord_memberchk(w, Neighbours), \+ ord_memberchk(sw, Neighbours).
can_move(Neighbours, e) :-
   \+ ord_memberchk(ne, Neighbours), \+ ord_memberchk(e, Neighbours), \+ ord_memberchk(se, Neighbours).

% Move to the best new position that is possible - this is only a proposal
move(Positions, Priorities, Position, NewPosition, 1) :-
   neighbours(Positions, Position, Neighbours),
   Neighbours \= [],
   member(Direction, Priorities),
   can_move(Neighbours, Direction),
   neighbour(Position, Direction-NewPosition), !.
move(_, _, Position, Position, 0).

% Add a proposal; if another proposal is also added for the same destination then
% remove both and add them as the original positions (there can be at most two
% proposals to a single location)
add_proposal(Position-NewPosition, ProposalSet, [Position-Position, OtherPosition-OtherPosition|TmpSet]) :-
   select(NewPosition-OtherPosition, ProposalSet, TmpSet), !.
add_proposal(Position-NewPosition, ProposalSet, [NewPosition-Position|ProposalSet]).

% Get whether any position changed from a change list
changed([], 0) :- !.
changed([1|_], 1) :- !.
changed([0|Rest], Changed) :- changed(Rest, Changed).

% Apply a single round
round_(Positions, Priorities, Position, Position-NewPosition, Changed) :-
   move(Positions, Priorities, Position, NewPosition, Changed).
round((Positions, Priorities), (NewPositions, NewPriorities), Changed) :-
   list_to_ord_set(Positions, SortedPositions),
   concurrent_maplist(round_(SortedPositions, Priorities), SortedPositions, Proposals, ChangedList),
   changed(ChangedList, Changed),
   foldl(add_proposal, Proposals, [], JoinedProposals),
   maplist([Pos-_, Pos]>>true, JoinedProposals, NewPositions),
   Priorities = [First|Rest],
   append(Rest, [First], NewPriorities).

% Apply n rounds
nrounds_(0, (Positions, _), Positions) :- !.
nrounds_(Num, (Positions, Priorities), FinalPositions) :-
   NextNum is Num - 1,
   round((Positions, Priorities), (NewPositions, NewPriorities), _),
   nrounds_(NextNum, (NewPositions, NewPriorities), FinalPositions).
nrounds(Num, Positions, FinalPositions) :-
   nrounds_(Num, (Positions, [n, s, w, e]), FinalPositions).

% Determine the number of empty tiles in a region
region(Positions, NumberEmpty) :-
   maplist([(X, Y), X, Y]>>true, Positions, Xs, Ys),
   min_list(Xs, XMin), max_list(Xs, XMax),
   min_list(Ys, YMin), max_list(Ys, YMax),
   length(Positions, NumberElves),
   NumberEmpty is (((YMax + 1) - YMin) * ((XMax + 1) - XMin)) - NumberElves.

% Run for a number of rounds and compute the number of empty tiles in a region
compute_empty(NumberRounds, StartPositions, NumberEmpty) :-
   nrounds(NumberRounds, StartPositions, FinalPositions),
   region(FinalPositions, NumberEmpty).

% Number of rounds until no change
rounds_until_idempotent(Num, PositionsPriorities, FinalNum) :-
   round(PositionsPriorities, NewPositionsPriorities, 1), !,
   NewNum is Num + 1,
   rounds_until_idempotent(NewNum, NewPositionsPriorities, FinalNum).
rounds_until_idempotent(Num, _, Num).
rounds_until_idempotent(Positions, Num) :-
   rounds_until_idempotent(1, (Positions, [n, s, w, e]), Num).

:- phrase_from_file(elves(Positions), '../input/day23.txt'),
   compute_empty(10, Positions, NumberEmpty),
   writeln(NumberEmpty),
   rounds_until_idempotent(Positions, Num),
   writeln(Num).

:- begin_tests(aoc202223).

sample(Positions) :-
   Positions = [
     (0, 4), (1, 2), (1, 3), (1, 4), (1, 6),
     (2, 0), (2, 4), (2, 6), (3, 1), (3, 5),
     (3, 6), (4, 0), (4, 2), (4, 3), (4, 4),
     (5, 0), (5, 1), (5, 3), (5, 5), (5, 6),
     (6, 1), (6, 4)
   ].

test(neighbours) :-
   sample(Positions),
   neighbours(Positions, (3, 5), [e, ne, nw, sw]),
   neighbours(Positions, (0, 4), Neighbours),
   can_move(Neighbours, n),
   can_move(Neighbours, e),
   \+ can_move(Neighbours, s),
   \+ can_move(Neighbours, w), !.

test(move) :-
   sample(Positions),
   move(Positions, [n, s, w, e], (0, 4), (-1, 4), 1),
   move(Positions, [n, s, w, e], (3, 5), (3, 5), 0).

test(add_proposal) :-
   add_proposal((0, 4)-(0, 0), [], Proposals),
   Proposals = [(0, 0)-(0, 4)],
   add_proposal((1, 2)-(0, 0), Proposals, [(1, 2)-(1, 2), (0, 4)-(0, 4)]).

test(round) :-
   sample(Positions),
   round((Positions, [n, s, w, e]), (NewPositions, [s, w, e, n]), _),
   NewPositions = [(7,4),(7,1),(5,6),(3,6),(5,5),(5,3),(5,1),(5,-1),(4,4),(3,3),(4,2),(4,-1),(3,5),(3,1),(2,7),(2,4),(1,0),(0,6),(1,5),(1,3),(0,2),(-1,4)].

test(nrounds) :-
   sample(Positions),
   nrounds(10, Positions, FinalPositions),
   FinalPositions = [(8,4),(8,1),(8,7),(6,6),(6,3),(6,1),(5,-1),(5,8),(3,6),(3,5),(4,3),(4,2),(3,-2),(2,9),(2,0),(2,6),(1,3),(0,4),(0,1),(0,-1),(-1,8),(-2,4)].

test(compute_empty) :-
   sample(Positions),
   compute_empty(10, Positions, 110).

test(rounds_until_idempotent) :-
   sample(Positions),
   rounds_until_idempotent(Positions, 20).

:- end_tests(aoc202223).

:- run_tests(aoc202223).

:- halt.
