% Advent of Code 2022 - Day 7
:- use_module(library(dcg/basics),     [eos/2, eol/2, whites/2, string_without/4, integer/3]).
:- use_module(library(dcg/high_order), [sequence/4]).
:- use_module(library(pio),            [phrase_from_file/2]).

% DCG for input file to extract a list of commands and their output
commands([])                 --> eos, !.
commands(Commands)           --> whites, eol, commands(Commands).
commands([Command|Commands]) --> whites, command(Command), commands(Commands).

command(cd(Directory)) --> "$ cd ", string_without("\n", DirectoryCodes), {string_codes(Directory, DirectoryCodes)}, eol.
command(ls(Listing))   --> "$ ls", eol, sequence(listing, Listing).

listing(file(Name, Size)) --> whites, integer(Size), " ", string_without("\n", NameCodes), {string_codes(Name, NameCodes)}, eol.
listing(dir(Name))        --> whites, "dir ", string_without("\n", NameCodes), {string_codes(Name, NameCodes)}, eol.

% Execute commands to construct tree
node_listing(file(Name, Size), file(Name, Size)).
node_listing(dir(Name), node(Name, _Children, _Size)).

exec_commands([], _, _).
exec_commands([cd("/")|Commands], CurrentNode, []) :- !,
   CurrentNode = node("/", _, _),
   exec_commands(Commands, CurrentNode, []).
exec_commands([cd("/")|Commands], _CurrentNode, PathSoFar) :- !,
   last(PathSoFar, NewCurrentNode),
   exec_commands(Commands, NewCurrentNode, []).
exec_commands([cd("..")|Commands], _CurrentNode, [ParentNode|RestOfPath]) :- !,
   exec_commands(Commands, ParentNode, RestOfPath).
exec_commands([cd(Directory)|Commands], CurrentNode, PathSoFar) :-
   CurrentNode = node(_Name, Children, _Size),
   NewCurrentNode = node(Directory, _NewChildren, _NewSize),
   member(NewCurrentNode, Children), !,
   exec_commands(Commands, NewCurrentNode, [CurrentNode|PathSoFar]).
exec_commands([ls(Listing)|Commands], CurrentNode, PathSoFar) :-
   CurrentNode = node(_Name, Children, _Size),
   maplist(node_listing, Listing, Children),
   exec_commands(Commands, CurrentNode, PathSoFar).

% Compute sizes
check_size(file(_Name, Size), Size).
check_size(node(_Name, Children, Size), Size) :-
   maplist(check_size, Children, ChildrenSizes),
   sum_list(ChildrenSizes, Size).

% Find sizes of nodes where node size satisfies a condition
check_dir_size(Size, =<, SizeAmount) :- Size =< SizeAmount.
check_dir_size(Size, >=, SizeAmount) :- Size >= SizeAmount.

find_size(node(_Name, _Children, Size), SizeComparison, SizeAmount, Size) :- check_dir_size(Size, SizeComparison, SizeAmount).
find_size(node(_Name, Children, _Size), SizeComparison, SizeAmount, Size) :- member(Child, Children), find_size(Child, SizeComparison, SizeAmount, Size).
find_sizes(RootNode, SizeComparison, SizeAmount, Sizes) :- findall(Size, find_size(RootNode, SizeComparison, SizeAmount, Size), Sizes).

% Find sizes given a list of commands
sizes_sum_for_commands(Commands, TotalSize) :-
   exec_commands(Commands, RootNode, []),
   check_size(RootNode, _Size),
   find_sizes(RootNode, =<, 100000, Sizes),
   sum_list(Sizes, TotalSize).

smallest_size_for_commands(Commands, SmallestSize) :-
   exec_commands(Commands, RootNode, []),
   check_size(RootNode, RootNodeSize),
   NeedSize is 30000000 + RootNodeSize - 70000000,
   find_sizes(RootNode, >=, NeedSize, Sizes),
   min_list(Sizes, SmallestSize).

:- phrase_from_file(commands(Commands), '../input/day07.txt'),
   sizes_sum_for_commands(Commands, TotalSize),
   writeln(TotalSize),
   smallest_size_for_commands(Commands, SmallestSize),
   writeln(SmallestSize).

% Examples to test the main predicate
:- begin_tests(aoc202207).

% Support quasi quotations for directory structure
:- use_module(library(quasi_quotations), [phrase_from_quasi_quotation/2]).
:- quasi_quotation_syntax(shell_listing).
shell_listing(Content, _Vars, _Dict, Listing) :-
   phrase_from_quasi_quotation(commands(Listing), Content).

test(sample_sequence_sum) :-
   Y = {|shell_listing||
        $ cd /
        $ ls
        dir a
        14848514 b.txt
        8504156 c.dat
        dir d
        $ cd a
        $ ls
        dir e
        29116 f
        2557 g
        62596 h.lst
        $ cd e
        $ ls
        584 i
        $ cd ..
        $ cd ..
        $ cd d
        $ ls
        4060174 j
        8033020 d.log
        5626152 d.ext
        7214296 k
        |},
   sizes_sum_for_commands(Y, 95437).

test(sample_sequence_smallest) :-
   Y = {|shell_listing||
        $ cd /
        $ ls
        dir a
        14848514 b.txt
        8504156 c.dat
        dir d
        $ cd a
        $ ls
        dir e
        29116 f
        2557 g
        62596 h.lst
        $ cd e
        $ ls
        584 i
        $ cd ..
        $ cd ..
        $ cd d
        $ ls
        4060174 j
        8033020 d.log
        5626152 d.ext
        7214296 k
        |},
   smallest_size_for_commands(Y, 24933642).

:- end_tests(aoc202207).

:- run_tests(aoc202207).

:- halt.
