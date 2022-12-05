% Advent of Code 2022 - Day 4
:- use_module(library(dcg/basics)).
:- use_module(library(pio),       [phrase_from_file/2]).
:- use_module(library(aggregate), [aggregate_all/3]).

% DCG for input file to turn into a list of assignments, each of which is a four tuple
% containing the min-max for each elf in order
assignments([]) --> eos, !.
assignments([Assignment|Assignments]) --> assignment(Assignment), assignments(Assignments).

assignment((FirstLower, FirstUpper, SecondLower, SecondUpper)) -->
   integer(FirstLower),
   string([0'-]),
   integer(FirstUpper),
   string([0',]),
   integer(SecondLower),
   string([0'-]),
   integer(SecondUpper),
   eol.

% Predicate to check whether one elf's assignment is contained within the other's
contained_within((FirstLower, FirstUpper, SecondLower, SecondUpper)) :-
   FirstLower =< SecondLower, FirstUpper >= SecondUpper, !.
contained_within((FirstLower, FirstUpper, SecondLower, SecondUpper)) :-
   SecondLower =< FirstLower, SecondUpper >= FirstUpper.

% Predicate that is true once for each time an assignment matches the contained_within
% predicate
valid_assignment(Assignments) :-
   member(Assignment, Assignments),
   contained_within(Assignment).

% Count the total number of assignments where one elf's is contained within the other's
count_valid_assignments(Count) :-
   phrase_from_file(assignments(Assignments), 'aoc202204.txt'),
   aggregate_all(count, valid_assignment(Assignments), Count).

:- count_valid_assignments(Count),
   write(Count), nl,
   halt.
