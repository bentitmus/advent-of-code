% Advent of Code 2022 - Day 6
:- use_module(library(dcg/basics), [eol/2, eos/2, nonblank/3]).
:- use_module(library(pio),        [phrase_from_file/2]).
:- use_module(library(clpfd),      [all_distinct/1]).

% DCG for input file to extract a list of char codes
char_codes([])           --> eol, eos, !.
char_codes([Code|Codes]) --> nonblank(Code), char_codes(Codes).

% Find the first index at which the N previously received consecutive characters are unique
find_unique_sequence(SequenceLength, List, Index, Index) :-
   length(Sequence, SequenceLength),
   append(Sequence, _, List),
   all_distinct(Sequence), !.
find_unique_sequence(SequenceLength, [_|Rest], Index, ReturnIndex) :-
   NewIndex is Index + 1,
   find_unique_sequence(SequenceLength, Rest, NewIndex, ReturnIndex).

find_unique_sequence(SequenceLength, List, Index) :-
   find_unique_sequence(SequenceLength, List, SequenceLength, Index).

% Get unique sequence index for the AoC input file for a given sequence length
unique_sequence_index(SequenceLength, Index) :-
   phrase_from_file(char_codes(CharCodeList), 'aoc202206.txt'),
   find_unique_sequence(SequenceLength, CharCodeList, Index).

:- unique_sequence_index(4, ShortIndex),
   writeln(ShortIndex),
   unique_sequence_index(14, LongIndex),
   writeln(LongIndex).

% Examples to test the main predicate
:- begin_tests(aoc202206).

test(first_sample_sequence) :-
   string_codes("mjqjpqmgbljsphdztnvjfqwrcgsmlb", Codes),
   find_unique_sequence(4, Codes, 7),
   find_unique_sequence(14, Codes, 19).

test(second_sample_sequence) :-
   string_codes("bvwbjplbgvbhsrlpgdmjqwftvncz", Codes),
   find_unique_sequence(4, Codes, 5),
   find_unique_sequence(14, Codes, 23).

test(third_sample_sequence) :-
   string_codes("nppdvjthqldpwncqszvftbrmjlhg", Codes),
   find_unique_sequence(4, Codes, 6),
   find_unique_sequence(14, Codes, 23).

test(fourth_sample_sequence) :-
   string_codes("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", Codes),
   find_unique_sequence(4, Codes, 10),
   find_unique_sequence(14, Codes, 29).

test(fifth_sample_sequence) :-
   string_codes("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", Codes),
   find_unique_sequence(4, Codes, 11),
   find_unique_sequence(14, Codes, 26).

:- end_tests(aoc202206).

:- run_tests(aoc202206).

:- halt.
