% Advent of Code 2022 - Day 25
:- use_module(library(dcg/basics),     [eos//0, eol//0, nonblanks//1]).
:- use_module(library(dcg/high_order), [sequence//2]).
:- use_module(library(pio),            [phrase_from_file/2]).

% DCG for input file to get a list of snafu strings
snafu_strings(Strings) --> sequence(snafu_string, Strings), eos, !.
snafu_string(String)   --> nonblanks(Codes), !, eol, {\+ length(Codes, 0), string_codes(String, Codes)}, !.

% Conversion between base 10 and snafu digits
snafu_digit(0'2, 2).
snafu_digit(0'1, 1).
snafu_digit(0'0, 0).
snafu_digit(0'-, -1).
snafu_digit(0'=, -2).

% Convert a snafu number to a base 10 number
snafu_codes([], Number, Number) :- !.
snafu_codes([Digit|Digits], Number, FinalNumber) :-
   snafu_digit(Digit, DigitNumber),
   NextNumber is (Number * 5) + DigitNumber,
   snafu_codes(Digits, NextNumber, FinalNumber).

% Convert a base 10 number to a snafu number
next_number(4, -1, 1) :- !.
next_number(3, -2, 1) :- !.
next_number(X,  X, 0).

snafu_numbers(0, Digits, Digits) :- !.
snafu_numbers(Number, Digits, FinalDigits) :-
   RequiredDigit is Number mod 5,
   next_number(RequiredDigit, Digit, Accumulator),
   NextNumber is (Number // 5) + Accumulator,
   snafu_numbers(NextNumber, [Digit|Digits], FinalDigits).

% Wrapper to convert between snafu and base 10 numbers
snafu_number(String, Number) :-
   string(String), !,
   string_codes(String, Codes),
   snafu_codes(Codes, 0, Number).
snafu_number(String, Number) :-
   var(String),
   snafu_numbers(Number, [], Digits),
   maplist([X, Y]>>snafu_digit(Y, X), Digits, DigitCodes),
   string_codes(String, DigitCodes).

% Compute the sum of a list of snafu numbers, with the result being a snafu
% number
compute_snafu_sum(SnafuStrings, SnafuSum) :-
   maplist(snafu_number, SnafuStrings, Numbers),
   sum_list(Numbers, Sum),
   snafu_number(SnafuSum, Sum).

:- phrase_from_file(snafu_strings(SnafuStrings), '2022/input/day25.txt'),
   compute_snafu_sum(SnafuStrings, SnafuSum),
   writeln(SnafuSum).

:- begin_tests(aoc202225).

test(snafu_number_to_number) :-
   snafu_number("1=-0-2", 1747),
   snafu_number("12111",   906),
   snafu_number("2=0=",    198),
   snafu_number("21",       11),
   snafu_number("2=01",    201),
   snafu_number("111",      31),
   snafu_number("20012",  1257),
   snafu_number("112",      32),
   snafu_number("1=-1=",   353),
   snafu_number("1-12",    107),
   snafu_number("12",        7),
   snafu_number("1=",        3),
   snafu_number("122",      37).

test(snafu_number_to_string) :-
   snafu_number(X01, 1747), X01 = "1=-0-2",
   snafu_number(X02,  906), X02 = "12111",
   snafu_number(X03,  198), X03 = "2=0=",
   snafu_number(X04,   11), X04 = "21",
   snafu_number(X05,  201), X05 = "2=01",
   snafu_number(X06,   31), X06 = "111",
   snafu_number(X07, 1257), X07 = "20012",
   snafu_number(X08,   32), X08 = "112",
   snafu_number(X09,  353), X09 = "1=-1=",
   snafu_number(X10,  107), X10 = "1-12",
   snafu_number(X11,    7), X11 = "12",
   snafu_number(X12,    3), X12 = "1=",
   snafu_number(X13,   37), X13 = "122".

test(compute_snafu_sum) :-
   SnafuNumbers = [
      "1=-0-2",
      "12111",
      "2=0=",
      "21",
      "2=01",
      "111",
      "20012",
      "112",
      "1=-1=",
      "1-12",
      "12",
      "1=",
      "122"
   ],
   compute_snafu_sum(SnafuNumbers, "2=-1=0").

:- end_tests(aoc202225).

:- run_tests(aoc202225).

:- halt.
