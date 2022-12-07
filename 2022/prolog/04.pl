% Advent of Code 2022 - Day 4
:- use_module(library(dcg/basics),     [eos/2, integer/3, string/3, eol/2, whites/2]).
:- use_module(library(dcg/high_order), [optional/4]).
:- use_module(library(pio),            [phrase_from_file/2]).
:- use_module(library(aggregate),      [aggregate_all/3]).

% DCG for input file to turn into a list of assignments, each of which is a four tuple
% containing the min-max for each elf in order
assignments([])                       --> optional(eol, {true}), whites, eos, !.
assignments([Assignment|Assignments]) --> optional(eol, {true}), prefix, assignment(Assignment), assignments(Assignments).

prefix_content --> whites, "|".
prefix         --> optional(prefix_content, {true}).

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

% Predicate to check whether one elf's assignment overlaps with the other's
overlaps((FirstLower, FirstUpper, SecondLower, _)) :-
   FirstLower =< SecondLower, FirstUpper >= SecondLower, !.
overlaps((FirstLower, _, SecondLower, SecondUpper)) :-
   SecondLower =< FirstLower, SecondUpper >= FirstLower.

% Predicate that is true once for each time an assignment matches the predicate
valid_assignment(Assignments, Predicate) :-
   member(Assignment, Assignments),
   call(Predicate, Assignment).

count_valid_assignments(Assignments, Predicate, Count) :-
   aggregate_all(count, valid_assignment(Assignments, Predicate), Count).

% Count the total number of assignments where one elf's is contained within the other's
count_valid_assignments(Count, Predicate) :-
   phrase_from_file(assignments(Assignments), '../input/04.txt'),
   count_valid_assignments(Assignments, Predicate, Count).

:- count_valid_assignments(WithinCount, contained_within),
   writeln(WithinCount),
   count_valid_assignments(OverlapCount, overlaps),
   writeln(OverlapCount).

:- begin_tests(aoc202204).

% Support quasi quotations for assignments
:- use_module(library(quasi_quotations), [phrase_from_quasi_quotation/2]).
:- quasi_quotation_syntax(assignments_listing).
assignments_listing(Content, _Vars, _Dict, Assignments) :-
   phrase_from_quasi_quotation(assignments(Assignments), Content).

test(contained_within_listing) :-
   Assignments = {|assignments_listing||
                  |2-4,6-8
                  |2-3,4-5
                  |5-7,7-9
                  |2-8,3-7
                  |6-6,4-6
                  |2-6,4-8
                  |},
   count_valid_assignments(Assignments, contained_within, 2).

test(overlap_listing) :-
   Assignments = {|assignments_listing||
                  |2-4,6-8
                  |2-3,4-5
                  |5-7,7-9
                  |2-8,3-7
                  |6-6,4-6
                  |2-6,4-8
                  |},
   count_valid_assignments(Assignments, overlaps, 4).

:- end_tests(aoc202204).

:- run_tests(aoc202204).

:- halt.
