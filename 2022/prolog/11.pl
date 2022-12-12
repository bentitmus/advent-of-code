% Advent of Code 2022 - Day 11
:- use_module(library(dcg/basics),     [eos//0, eol//0, whites//0, integer//1]).
:- use_module(library(dcg/high_order), [sequence//2, optional//2]).
:- use_module(library(pio),            [phrase_from_file/2]).

% DCG for input file to get a list of monkeys
% Each monkey is a compound with:
%   monkey(Worries:list, Action:compound, Check:integer, IfTrue:integer, IfFalse:integer, Activity:integer)
monkeys(Monkeys) --> sequence(monkey, Monkeys), eos.

monkey(monkey(Worries, Action, Check, IfTrue, IfFalse, 0)) -->
   optional(eol, {true}),
   "Monkey ", integer(_), ":", eol,
   whites, "Starting items: ", starting_worries(Worries),
   whites, "Operation: new = old ", action(Action), eol,
   whites, "Test: divisible by ", integer(Check), eol,
   whites, "If true: throw to monkey ", integer(IfTrue), eol,
   whites, "If false: throw to monkey ", integer(IfFalse), eol.

starting_worries([Worry])         --> integer(Worry), eol.
starting_worries([Worry|Worries]) --> integer(Worry), ", ", starting_worries(Worries).

action(square)      --> "* old".
action(mult(Value)) --> "* ", integer(Value).
action(plus(Value)) --> "+ ", integer(Value).

% Apply an action to update the worry value
update_worry_action(square,      Worry, NewWorry) :- NewWorry is Worry * Worry.
update_worry_action(mult(Value), Worry, NewWorry) :- NewWorry is Worry * Value.
update_worry_action(plus(Value), Worry, NewWorry) :- NewWorry is Worry + Value.

% Determine which monkey should take the item after it is processed
determine_new_monkey(Value, Check, IfTrue, _,      IfTrue) :- 0 is Value mod Check, !.
determine_new_monkey(_,     _,     _,     IfFalse, IfFalse).

% Handle an item from a monkey, determining which monkey it should go to and what the
% new worry value is
handle_item(UpdatePredicate,
            monkey([Item|Items], Action, Check, IfTrue, IfFalse, ItemsSoFar),
            monkey(Items,        Action, Check, IfTrue, IfFalse, NewItemsSoFar),
            item_update(NewItem, NewMonkeyForItem)) :- !,
   NewItemsSoFar is ItemsSoFar + 1,
   update_worry_action(Action, Item, ItemAfterAction),
   call(UpdatePredicate, ItemAfterAction, NewItem),
   determine_new_monkey(NewItem, Check, IfTrue, IfFalse, NewMonkeyForItem).

% Handle all items from a monkey, returning a list of updated items and which monkey
% they should go to
handle_items(UpdatedPredicate, Monkey, UpdatedMonkey, [ItemUpdate|ItemUpdates]) :-
   handle_item(UpdatedPredicate, Monkey, NewMonkey, ItemUpdate), !,
   handle_items(UpdatedPredicate, NewMonkey, UpdatedMonkey, ItemUpdates).
handle_items(_, Monkey, Monkey, []).

% Process the updates after a monkey's items have been handled, putting the new items
% with the appropriate monkey
% This is not particularly elegant as it requires indexing into the monkey list (hence
% the use of nth0)
process_updates([], Monkeys, Monkeys).
process_updates([item_update(Item, NewMonkeyForItem)|ItemUpdates], Monkeys, FinalMonkeys) :-
   nth0(NewMonkeyForItem, Monkeys, monkey(Items, Action, Check, IfTrue, IfFalse, ItemsSoFar), RestMonkeys),
   append(Items, [Item], NewItems),
   nth0(NewMonkeyForItem, UpdatedMonkeys, monkey(NewItems, Action, Check, IfTrue, IfFalse, ItemsSoFar), RestMonkeys),
   process_updates(ItemUpdates, UpdatedMonkeys, FinalMonkeys).

% Run one round of the simulation handling all items for all monkeys in turn
% Because the monkeys need to be updated using indexing, this is not particularly
% elegant and uses nth0
round(_,               Index, Monkeys, Monkeys)      :- length(Monkeys, Index), !.
round(UpdatePredicate, Index, Monkeys, FinalMonkeys) :-
   % Extract the monkey from the list using indexing - we require a complete list of
   % up-to-date monkeys to process the item updates
   nth0(Index, Monkeys, Monkey, RestMonkeys),
   handle_items(UpdatePredicate, Monkey, UpdatedMonkey, ItemUpdates),
   nth0(Index, MonkeysWithUpdatedMonkey, UpdatedMonkey, RestMonkeys),
   process_updates(ItemUpdates, MonkeysWithUpdatedMonkey, UpdatedMonkeys),
   NewIndex is Index + 1,
   round(UpdatePredicate, NewIndex, UpdatedMonkeys, FinalMonkeys).
round(UpdatePredicate, Monkeys, UpdatedMonkeys) :- round(UpdatePredicate, 0, Monkeys, UpdatedMonkeys).

% Run the simulation for a given number of rounds
rounds(_, 0, Monkeys, Monkeys) :- !.
rounds(UpdatePredicate, Rounds, Monkeys, FinalMonkeys) :-
   round(UpdatePredicate, Monkeys, MonkeysAfterRound),
   NewRounds is Rounds - 1,
   rounds(UpdatePredicate, NewRounds, MonkeysAfterRound, FinalMonkeys).

% Compute the sum of the activity numbers of the two most active monkeys
most_active_sum(Monkeys, Sum) :-
   maplist([monkey(_, _, _, _, _, Activity), Activity]>>true, Monkeys, Activities),
   sort(0, @>=, Activities, [First, Second|_]),
   Sum is First * Second.

% Compute the LCM for a list of numbers
lcm([], 1).
lcm([Check|Checks], LcmSoFar) :-
   lcm(Checks, LcmForItem),
   LcmSoFar is lcm(Check, LcmForItem).

% Takes a list of monkeys and computes the LCM of the 'check' values
% The worry values are then stored mod this divisor
calculate_divisor(Monkeys, Divisor) :-
   maplist([monkey(_, _, Check, _, _, _), Check]>>true, Monkeys, Divisors),
   lcm(Divisors, Divisor).

% Update predicate to reduce the worry by dividing by three
% Used in part 1
update_worry_div_3(Worry, NewWorry) :-
   NewWorry is Worry // 3.

% Update predicate to reduce the worry modulo the LCM of the check values
% Used in part 2
update_worry_mod_divisor(Divisor, Worry, NewWorry) :-
   NewWorry is Worry mod Divisor.

:- phrase_from_file(monkeys(Monkeys), '../input/11.txt'),
   rounds(update_worry_div_3, 20, Monkeys, MonkeysAfterDiv3),
   most_active_sum(MonkeysAfterDiv3, SumAfterDiv3),
   writeln(SumAfterDiv3),
   calculate_divisor(Monkeys, Divisor),
   rounds(update_worry_mod_divisor(Divisor), 10000, Monkeys, MonkeysAfterDivisors),
   most_active_sum(MonkeysAfterDivisors, SumAfterDivisors),
   writeln(SumAfterDivisors).

:- begin_tests(aoc202211).

sample([Monkey0, Monkey1, Monkey2, Monkey3]) :-
   Monkey0 = monkey([79, 98],
                    mult(19),
                    23,
                    2,
                    3,
                    0),
   Monkey1 = monkey([54, 65, 75, 74],
                    plus(6),
                    19,
                    2,
                    0,
                    0),
   Monkey2 = monkey([79, 60, 97],
                    square,
                    13,
                    1,
                    3,
                    0),
   Monkey3 = monkey([74],
                    plus(3),
                    17,
                    0,
                    1,
                    0).

test(sample_part_1) :-
   sample(Monkeys),
   rounds(update_worry_div_3, 20, Monkeys, MonkeysAfterRounds),
   most_active_sum(MonkeysAfterRounds, 10605).

test(sample_part_2) :-
   sample(Monkeys),
   calculate_divisor(Monkeys, Divisor),
   rounds(update_worry_mod_divisor(Divisor), 10000, Monkeys, MonkeysAfterRounds),
   most_active_sum(MonkeysAfterRounds, 2713310158).

:- end_tests(aoc202211).

:- run_tests(aoc202211).

:- halt.
