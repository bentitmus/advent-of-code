% Advent of Code 2022 - Day 10
:- use_module(library(dcg/basics),     [eos//0, eol//0, whites//0, integer//1]).
:- use_module(library(dcg/high_order), [sequence//2, optional//2]).
:- use_module(library(pio),            [phrase_from_file/2]).

% DCG for input file to create list of instructions
% addx is treated as a noop and then an addx to make calculation easier
instrs(Instructions) --> optional(eol, {true}), sequence(instruction, InstrList), {flatten(InstrList, Instructions)}, whites, eos.

prefix --> whites, optional("|", {true}).

instruction(noop) --> prefix, "noop", eol.
instruction([noop, addx(Amount)]) --> prefix, "addx ", integer(Amount), eol.

% Execute a single instruction, updating the value in the X register
exec_instr(noop, X, X).
exec_instr(addx(Amount), X, Y) :- Y is X + Amount.

% Execute all instructions applying a predicate to update the accumulator each
% time; the accumulator is updated on stack return
exec_instrs(_, [], _, _, DefaultValue, DefaultValue) :- !.
exec_instrs(Predicate, [Instr|Instrs], Index, X, DefaultValue, RetValue) :-
   exec_instr(Instr, X, NewX),
   NewIndex is Index + 1,
   exec_instrs(Predicate, Instrs, NewIndex, NewX, DefaultValue, Value),
   call(Predicate, Index, X, Value, RetValue).
exec_instrs(Predicate, Instrs, DefaultValue, RetValue) :-
   exec_instrs(Predicate, Instrs, 1, 1, DefaultValue, RetValue).

% Update the signal strength for part 1
update_signal_strength(Index, X, PrevSignal, NewSignal) :-
   0 is (Index - 20) mod 40, !,
   NewSignal is PrevSignal + X * Index.
update_signal_strength(_, _, Signal, Signal).

% Add a pixel value for part 2
add_pixel(Index, X, RestOfList, [0'#|RestOfList]) :-
   InRange is abs(((Index - 1) mod 40) - X),
   InRange < 2, !.
add_pixel(_, _, RestOfList, [0'.|RestOfList]).

% Split the pixels into lines 40 pixels long
split_pixels([], []) :- !.
split_pixels(List, [Str|Lines]) :-
   length(Line, 40),
   append(Line, Rest, List),
   string_codes(Str, Line),
   split_pixels(Rest, Lines).

:- phrase_from_file(instrs(Instrs), '2022/input/day10.txt'),
   exec_instrs(update_signal_strength, Instrs, 0, SignalStrength),
   writeln(SignalStrength),
   exec_instrs(add_pixel, Instrs, [], AllPixels),
   split_pixels(AllPixels, Screen),
   maplist(writeln, Screen).

% Examples to test the main predicate
:- begin_tests(aoc202210).

:- use_module(library(quasi_quotations), [phrase_from_quasi_quotation/2]).
:- quasi_quotation_syntax(instr_listing).
instr_listing(Content, _Vars, _Dict, Listing) :-
   phrase_from_quasi_quotation(instrs(Listing), Content).

sample(X) :-
   X = {|instr_listing||
        |addx 15
        |addx -11
        |addx 6
        |addx -3
        |addx 5
        |addx -1
        |addx -8
        |addx 13
        |addx 4
        |noop
        |addx -1
        |addx 5
        |addx -1
        |addx 5
        |addx -1
        |addx 5
        |addx -1
        |addx 5
        |addx -1
        |addx -35
        |addx 1
        |addx 24
        |addx -19
        |addx 1
        |addx 16
        |addx -11
        |noop
        |noop
        |addx 21
        |addx -15
        |noop
        |noop
        |addx -3
        |addx 9
        |addx 1
        |addx -3
        |addx 8
        |addx 1
        |addx 5
        |noop
        |noop
        |noop
        |noop
        |noop
        |addx -36
        |noop
        |addx 1
        |addx 7
        |noop
        |noop
        |noop
        |addx 2
        |addx 6
        |noop
        |noop
        |noop
        |noop
        |noop
        |addx 1
        |noop
        |noop
        |addx 7
        |addx 1
        |noop
        |addx -13
        |addx 13
        |addx 7
        |noop
        |addx 1
        |addx -33
        |noop
        |noop
        |noop
        |addx 2
        |noop
        |noop
        |noop
        |addx 8
        |noop
        |addx -1
        |addx 2
        |addx 1
        |noop
        |addx 17
        |addx -9
        |addx 1
        |addx 1
        |addx -3
        |addx 11
        |noop
        |noop
        |addx 1
        |noop
        |addx 1
        |noop
        |noop
        |addx -13
        |addx -19
        |addx 1
        |addx 3
        |addx 26
        |addx -30
        |addx 12
        |addx -1
        |addx 3
        |addx 1
        |noop
        |noop
        |noop
        |addx -9
        |addx 18
        |addx 1
        |addx 2
        |noop
        |noop
        |addx 9
        |noop
        |noop
        |noop
        |addx -1
        |addx 2
        |addx -37
        |addx 1
        |addx 3
        |noop
        |addx 15
        |addx -21
        |addx 22
        |addx -6
        |addx 1
        |noop
        |addx 2
        |addx 1
        |noop
        |addx -10
        |noop
        |noop
        |addx 20
        |addx 1
        |addx 2
        |addx 2
        |addx -6
        |addx -11
        |noop
        |noop
        |noop
        |}.

test(sample_for_part_1) :-
   sample(Instrs),
   exec_instrs(update_signal_strength, Instrs, 0, 13140).

test(sample_for_part_2) :-
   sample(Instrs),
   exec_instrs(add_pixel, Instrs, [], AllPixels),
   split_pixels(AllPixels, Screen),
   Screen = [
     "##..##..##..##..##..##..##..##..##..##..",
     "###...###...###...###...###...###...###.",
     "####....####....####....####....####....",
     "#####.....#####.....#####.....#####.....",
     "######......######......######......####",
     "#######.......#######.......#######....."
   ].

:- end_tests(aoc202210).

:- run_tests(aoc202210).

:- halt.
