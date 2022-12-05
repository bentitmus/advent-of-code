% Advent of Code 2022 - Day 5
:- use_module(library(dcg/basics)).
:- use_module(library(pio),       [phrase_from_file/2]).
:- use_module(library(apply),     [maplist/3, foldl/4]).
:- use_module(library(clpfd),     [transpose/2]).

% DCG for input file to extract an initial crates set-up and the moves that need to be applied
% Crates is a list where each item in the list is a list from top to bottom of the crates in that
% column
% Moves is a list of moves which are three tuples with the number of crates to move, the from
% column, and the to column (both 1 indexed)
puzzle(Crates, Moves)            --> all_crates(Crates), eol, moves(Moves).

crate(0)                         --> "   ".
crate(Crate)                     --> "[", nonblank(Crate), "]".
crates_line([Crate|Crates])      --> crate(Crate), " ", crates_line(Crates), !.
crates_line([Crate])             --> crate(Crate).
crates([CratesLine|CratesLines]) --> crates_line(CratesLine), eol, crates(CratesLines).
crates([CratesLine])             --> crates_line(CratesLine), eol.
numbers_line                     --> whites, eol.
numbers_line                     --> whites, integer(_), numbers_line.
all_crates(Crates)               --> crates(Crates), numbers_line.

moves([])                         --> eos.
moves([(Number, From, To)|Moves]) -->
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
first_item(List, Item) :- member(Item, List).

% Determine the final "top" crates for the given permuter predicate, giving the string as an output
final_crates(Permuter, String) :-
   phrase_from_file(puzzle(RawCrates, Moves), 'aoc202205.txt'),
   prepare_crates(RawCrates, Crates),
   apply_moves(Permuter, Crates, Moves, FinalCrates),
   maplist(first_item, FinalCrates, FinalItems),
   string_codes(String, FinalItems).

:- final_crates(reverse, CrateMover9000),
   write(CrateMover9000), nl,
   final_crates(=, CrateMover9001),
   write(CrateMover9001), nl,
   halt.
