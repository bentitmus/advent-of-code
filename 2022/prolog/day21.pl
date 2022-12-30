% Advent of Code 2022 - Day 21
:- use_module(library(dcg/basics),     [eos//0, eol//0, integer//1, nonblanks//1]).
:- use_module(library(dcg/high_order), [sequence//2]).
:- use_module(library(pio),            [phrase_from_file/2]).

% DCG for input file to get list of monkeys
monkeys(Monkeys) --> sequence(monkey, Monkeys), eos.
monkey(monkey(Name, Value))                    --> monkey_name(Name), ": ", integer(Value), eol.
monkey(monkey(Name, plus, Monkey1, Monkey2))   --> monkey_name(Name), ": ", monkey_name(Monkey1), " + ", monkey_name(Monkey2), eol.
monkey(monkey(Name, minus, Monkey1, Monkey2))  --> monkey_name(Name), ": ", monkey_name(Monkey1), " - ", monkey_name(Monkey2), eol.
monkey(monkey(Name, mult, Monkey1, Monkey2))   --> monkey_name(Name), ": ", monkey_name(Monkey1), " * ", monkey_name(Monkey2), eol.
monkey(monkey(Name, divide, Monkey1, Monkey2)) --> monkey_name(Name), ": ", monkey_name(Monkey1), " / ", monkey_name(Monkey2), eol.
monkey_name(Name) --> [A, B, C, D], {atom_codes(Name, [A, B, C, D])}.

% Perform the arithmetic operation based on what is known - require two
% instantiated terms for each operation
perform_op(plus, A, B, Result)   :- var(Result), Result is A + B.
perform_op(minus, A, B, Result)  :- var(Result), Result is A - B.
perform_op(mult, A, B, Result)   :- var(Result), Result is A * B.
perform_op(divide, A, B, Result) :- var(Result), Result is A // B.
perform_op(plus, A, B, Result)   :- var(A),      A      is Result - B.
perform_op(minus, A, B, Result)  :- var(A),      A      is Result + B.
perform_op(mult, A, B, Result)   :- var(A),      A      is Result // B.
perform_op(divide, A, B, Result) :- var(A),      A      is Result * B.
perform_op(plus, A, B, Result)   :- var(B),      B      is Result - A.
perform_op(minus, A, B, Result)  :- var(B),      B      is A - Result.
perform_op(mult, A, B, Result)   :- var(B),      B      is Result // A.
perform_op(divide, A, B, Result) :- var(B),      B      is A // Result.

% Try and use the given monkey value/operation to compute anything that is
% currently unknown and add new values to the database
:- dynamic monkey/2.

handle_monkey(monkey(Name, Value)) :-
   assertz(monkey(Name, Value)).
handle_monkey(monkey(Name, Operation, Monkey1, Monkey2)) :-
   monkey(Monkey1, Value1), monkey(Monkey2, Value2),
   perform_op(Operation, Value1, Value2, Result),
   assertz(monkey(Name, Result)).
handle_monkey(monkey(Name, Operation, Monkey1, Monkey2)) :-
   monkey(Name, Result), monkey(Monkey1, Value1),
   perform_op(Operation, Value1, Value2, Result),
   assertz(monkey(Monkey2, Value2)).
handle_monkey(monkey(Name, Operation, Monkey1, Monkey2)) :-
   monkey(Name, Result), monkey(Monkey2, Value2),
   perform_op(Operation, Value1, Value2, Result),
   assertz(monkey(Monkey1, Value1)).
handle_monkey(monkey(_, equal, Monkey1, Monkey2)) :-
   monkey(Monkey1, Value), assertz(monkey(Monkey2, Value)).
handle_monkey(monkey(_, equal, Monkey1, Monkey2)) :-
   monkey(Monkey2, Value), assertz(monkey(Monkey1, Value)).

% Handle all monkeys possible and return the list of unhandled monkeys
add_monkeys([], Unhandled, Unhandled).
add_monkeys([Monkey|Monkeys], Unhandled, ReturnMonkeys) :-
   handle_monkey(Monkey), !,
   add_monkeys(Monkeys, Unhandled,ReturnMonkeys).
add_monkeys([Monkey|Monkeys], Unhandled, ReturnMonkeys) :-
   add_monkeys(Monkeys, [Monkey|Unhandled], ReturnMonkeys).

% Repeatedly handle all monkeys in the list until no further progress can be
% made, then return the list of unhandled monkeys
reduce(Monkeys, Unhandled) :-
   add_monkeys(Monkeys, [], NewUnhandled),
   length(Monkeys, Length1),
   length(NewUnhandled, Length2),
   Length1 \== Length2, !,
   reduce(NewUnhandled, Unhandled).
reduce(Monkeys, Monkeys).

% Find the value of a particular monkey by processing the list of monkeys and
% then looking up the value of that monkey; clean up afterwards
find_monkey(Monkeys, Monkey, Value) :-
   reduce(Monkeys, []),
   monkey(Monkey, Value),
   retractall(monkey(_, _)).

% Change the monkeys for part 2 so the root monkey tests equality and the humn
% monkey is removed
change_specials(List, NewList) :-
   select(monkey(root, plus, Monkey1, Monkey2), List, TmpList1),
   select(monkey(root, equal, Monkey1, Monkey2), TmpList2, TmpList1),
   select(monkey(humn, _), TmpList2, NewList), !.

:- phrase_from_file(monkeys(Monkeys), '../input/day21.txt'),
   find_monkey(Monkeys, root, Root),
   writeln(Root),
   change_specials(Monkeys, MonkeysWithoutSpecials),
   find_monkey(MonkeysWithoutSpecials, humn, Humn),
   writeln(Humn).

:- begin_tests(aoc202221).

sample(Monkeys) :-
   Monkeys = [
      monkey(root, plus, pppw, sjmn),
      monkey(dbpl, 5),
      monkey(cczh, plus, sllz, lgvd),
      monkey(zczc, 2),
      monkey(ptdq, minus, humn, dvpt),
      monkey(dvpt, 3),
      monkey(lfqf, 4),
      monkey(humn, 5),
      monkey(ljgn, 2),
      monkey(sjmn, mult, drzm, dbpl),
      monkey(sllz, 4),
      monkey(pppw, divide, cczh, lfqf),
      monkey(lgvd, mult, ljgn, ptdq),
      monkey(drzm, minus, hmdt, zczc),
      monkey(hmdt, 32)
   ].

test(sample_part1) :-
   sample(Monkeys),
   find_monkey(Monkeys, root, 152).

test(sample_part2) :-
   sample(Monkeys),
   change_specials(Monkeys, MonkeysWithSpecials),
   find_monkey(MonkeysWithSpecials, humn, 301).

:- end_tests(aoc202221).

:- run_tests(aoc202221).

:- halt.
