% Advent of Code 2022 - Day 5
:- use_module(library(dcg/basics),     [eol/2, nonblank/3, whites/2, integer/3, eos/2]).
:- use_module(library(dcg/high_order), [optional/4]).
:- use_module(library(pio),            [phrase_from_file/2]).
:- use_module(library(apply),          [maplist/3, foldl/4]).
:- use_module(library(clpfd),          [transpose/2]).

% DCG for input file to extract an initial crates set-up and the moves that need to be applied
% Crates is a list where each item in the list is a list from top to bottom of the crates in that
% column
% Moves is a list of moves which are three tuples with the number of crates to move, the from
% column, and the to column (both 1 indexed)
puzzle(Crates, Moves)             --> optional(eol, {true}),
                                      all_crates(Crates),
                                      prefix, eol,
                                      moves(Moves).
prefix_content                    --> whites, "|".
prefix                            --> optional(prefix_content, {true}).

crate(0)                          --> "   ".
crate(Crate)                      --> "[", nonblank(Crate), "]".
crates_line([Crate|Crates])       --> crate(Crate), " ", crates_line(Crates), !.
crates_line([Crate])              --> crate(Crate).
crates([CratesLine|CratesLines])  --> prefix, crates_line(CratesLine), eol, crates(CratesLines).
crates([CratesLine])              --> prefix, crates_line(CratesLine), eol.
numbers_line                      --> prefix, whites, eol.
numbers_line                      --> prefix, whites, integer(_), numbers_line.
all_crates(Crates)                --> crates(Crates), numbers_line.

moves([])                         --> whites, eos.
moves([(Number, From, To)|Moves]) -->
   prefix,
   "move ", integer(Number),
   " from ", integer(From),
   " to ", integer(To),
   eol, moves(Moves).

% Helper predicate to delete leading 0 from all lists
delete0(List1, List2) :- delete(List1, 0, List2).

% Prepare the crates from that provided by the DCG by transposing and removing the leading 0s
prepare_crates(Crates, PreparedCrates) :-
   transpose(Crates, CratesTransposed),
   maplist(delete0, CratesTransposed, PreparedCrates).

% Execute move implementation
% +Index, +Move, +InputCrates, -OutputCrates, -ToPtr
% Uses 1 based indexing, and uses a difference list for the new ToCrate which is unified by the
% FromCrate clause
% Indexes are reassigned to 0 when the necessary work has been done

% When the From and To index are 0, there's no point in continuing to recurse through the list so end now
exec_move(_, (_, 0, 0), _, Crates, Crates, _) :- !.

% When we hit the FromCrate we should update the ToCrate difference list and remove the elements from
% the FromCrate
exec_move(Index, (Number, Index, To), Permuter, [FromCrate|Crates], [UpdatedFromCrate|UpdatedCrates], ToCrateItems-ToDiff) :-
   % Create a list of the right length and extract these elements using append/3
   length(FromCrateItems, Number),
   append(FromCrateItems, UpdatedFromCrate, FromCrate),
   % Call the permuter predicate to manipulate the list due to the crane
   call(Permuter, FromCrateItems, ReversedFromCrateItems),
   % Put the hole in the end of the difference list
   append(ReversedFromCrateItems, ToDiff, ToCrateItems),
   NewIndex is Index + 1,
   exec_move(NewIndex, (Number, 0, To), Permuter, Crates, UpdatedCrates, ToCrateItems-ToDiff), !.

% When we hit the ToCrate unify the result with the difference list which will be assigned in the
% FromCrate clause above
exec_move(Index, (Number, From, Index), Permuter, [ToCrate|Crates], [UpdatedToCrate|UpdatedCrates], UpdatedToCrate-ToCrate) :-
   NewIndex is Index + 1,
   exec_move(NewIndex, (Number, From, 0), Permuter, Crates, UpdatedCrates, UpdatedToCrate-ToCrate), !.

% If we don't hit either the From or ToCrates then just recurse
exec_move(Index, Move, Permuter, [Crate|Crates], [Crate|UpdatedCrates], Pointers) :-
   NewIndex is Index + 1,
   exec_move(NewIndex, Move, Permuter, Crates, UpdatedCrates, Pointers).

% Execute move for the given move and initial crates starting position
% The permuter used in the move is supplied along with the crates so it can be used in a foldl
exec_move(Move, (Permuter, Crates), (Permuter, UpdatedCrates)) :-
   exec_move(1, Move, Permuter, Crates, UpdatedCrates, _).

% Apply all moves to the initial starting crates where the crane uses the permuter predicate
apply_moves(Permuter, Crates, Moves, FinalCrates) :-
   foldl(exec_move, Moves, (Permuter, Crates), (_, FinalCrates)).

% Helper predicate to extract the first item from each list
first_item([Item|_], Item).

% Determine the final "top" crates for the given permuter predicate, giving the string as an output
final_crates(RawCrates, Moves, Permuter, String) :-
   prepare_crates(RawCrates, Crates),
   apply_moves(Permuter, Crates, Moves, FinalCrates),
   maplist(first_item, FinalCrates, FinalItems),
   string_codes(String, FinalItems).

final_crates_from_file(Permuter, String) :-
   phrase_from_file(puzzle(RawCrates, Moves), '../input/day05.txt'),
   final_crates(RawCrates, Moves, Permuter, String).

:- final_crates_from_file(reverse, CrateMover9000),
   writeln(CrateMover9000),
   final_crates_from_file(=, CrateMover9001),
   writeln(CrateMover9001).

:- begin_tests(aoc202205).

% Support quasi quotations for crates
:- use_module(library(quasi_quotations), [phrase_from_quasi_quotation/2]).
:- quasi_quotation_syntax(crates_listing).
crates_listing(Content, _Vars, _Dict, (Crates, Moves)) :-
   phrase_from_quasi_quotation(puzzle(Crates, Moves), Content).

test(craneMover9000_listing) :-
   (Crates, Moves) = {|crates_listing||
                      |    [D]    
                      |[N] [C]    
                      |[Z] [M] [P]
                      | 1   2   3
                      |
                      |move 1 from 2 to 1
                      |move 3 from 1 to 3
                      |move 2 from 2 to 1
                      |move 1 from 1 to 2
                      |},
   final_crates(Crates, Moves, reverse, "CMZ").

test(crateMover9001_listing) :-
   (Crates, Moves) = {|crates_listing||
                      |    [D]    
                      |[N] [C]    
                      |[Z] [M] [P]
                      | 1   2   3
                      |
                      |move 1 from 2 to 1
                      |move 3 from 1 to 3
                      |move 2 from 2 to 1
                      |move 1 from 1 to 2
                      |},
   final_crates(Crates, Moves, =, "MCD").

:- end_tests(aoc202205).

:- run_tests(aoc202205).

:- halt.
