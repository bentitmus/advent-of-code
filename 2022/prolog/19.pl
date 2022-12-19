% Advent of Code 2022 - Day 19
:- use_module(library(dcg/basics),     [eos//0, eol//0, integer//1, nonblanks//1]).
:- use_module(library(dcg/high_order), [sequence//2]).
:- use_module(library(pio),            [phrase_from_file/2]).

% DCG for input file to get list of blueprints
blueprints(Blueprints) --> sequence(blueprint, Blueprints), eos.
blueprint(blueprint(Ore, Clay, (ObsidianOre, ObsidianClay), (GeodeOre, GeodeObsidian))) -->
   "Blueprint ", integer(_), ": Each ore robot costs ", integer(Ore), " ore. ",
   "Each clay robot costs ", integer(Clay), " ore. ",
   "Each obsidian robot costs ", integer(ObsidianOre), " ore and ", integer(ObsidianClay), " clay. ",
   "Each geode robot costs ", integer(GeodeOre), " ore and ", integer(GeodeObsidian), " obsidian.", eol.

% Keep track of the maximum number of geodes that could be opened if we build a
% new geode robot each turn from now on (used to prune search tree)
% This is just 1/2 n * (n - 1), but since we need to calculate it for each value
% it is easiest to recurse
:- dynamic add_geodes/2.

assert_add_geodes(1) :- !,
   assertz(add_geodes(1, 0)).
assert_add_geodes(N) :-
   Next is N - 1,
   assert_add_geodes(Next),
   add_geodes(Next, Amount),
   NewAmount is Amount + Next,
   assertz(add_geodes(N, NewAmount)).

:- assert_add_geodes(32).

% Determine how many resources would be added if a certain amount of time elapses
% using the robots we have so far
gather(NumMinutes, robots(OrRob, ClRob, ObRob), res(Or, Cl, Ob), res(NewOr, NewCl, NewOb)) :-
   NewOr is Or + (OrRob * NumMinutes),
   NewCl is Cl + (ClRob * NumMinutes),
   NewOb is Ob + (ObRob * NumMinutes).

% In the graph search the transitions between the states are from building one
% robot to the next
% For each of these, work out the amount of time needed to collect the resources
% needed and just move to that time, remembering to add 1 for the time to build
% the robot
% For geodes, just count how many geodes would be harvested instead of keeping
% track of the resources so far (as they aren't consumed)
move(blueprint(_, _, _, (OrReq, ObReq)),
     _,
     state(NumMoves, robots(OrRob, ClRob, ObRob), Res, Geodes),
     state(NewNumMoves, robots(OrRob, ClRob, ObRob), res(NewOr, NewCl, NewOb), NewGeodes)) :-
   ObRob > 0,
   Res = res(Or, _, Ob),
   TimeNeeded is max(max(ceil((OrReq - Or) / OrRob), ceil((ObReq - Ob) / ObRob)), 0) + 1,
   NewNumMoves is NumMoves - TimeNeeded,
   NewNumMoves > 0,
   gather(TimeNeeded, robots(OrRob, ClRob, ObRob), Res, res(OrBC, NewCl, ObBC)),
   NewOr is OrBC - OrReq,
   NewOb is ObBC - ObReq,
   NewGeodes is Geodes + NewNumMoves.
move(blueprint(_, _, (OrReq, ClReq), _),
     max_robots(_, _, MaxObRob),
     state(NumMoves, robots(OrRob, ClRob, ObRob), Res, Geodes),
     state(NewNumMoves, robots(OrRob, ClRob, NewObRob), res(NewOr, NewCl, NewOb), Geodes)) :-
   ClRob > 0,
   ObRob < MaxObRob,
   NewObRob is ObRob + 1,
   Res = res(Or, Cl, _),
   TimeNeeded is max(max(ceil((OrReq - Or) / OrRob), ceil((ClReq - Cl) / ClRob)), 0) + 1,
   NewNumMoves is NumMoves - TimeNeeded,
   NewNumMoves > 0,
   gather(TimeNeeded, robots(OrRob, ClRob, ObRob), Res, res(OrBC, ClBC, NewOb)),
   NewOr is OrBC - OrReq,
   NewCl is ClBC - ClReq.
move(blueprint(_, OrReq, _, _),
     max_robots(_, MaxClRob, _),
     state(NumMoves, robots(OrRob, ClRob, ObRob), Res, Geodes),
     state(NewNumMoves, robots(OrRob, NewClRob, ObRob), res(NewOr, NewCl, NewOb), Geodes)) :-
   ClRob < MaxClRob,
   NewClRob is ClRob + 1,
   Res = res(Or, _, _),
   TimeNeeded is max(ceil((OrReq - Or) / OrRob), 0) + 1,
   NewNumMoves is NumMoves - TimeNeeded,
   NewNumMoves > 0,
   gather(TimeNeeded, robots(OrRob, ClRob, ObRob), Res, res(OrBC, NewCl, NewOb)),
   NewOr is OrBC - OrReq.
move(blueprint(OrReq, _, _, _),
     max_robots(MaxOrRob, _, _),
     state(NumMoves, robots(OrRob, ClRob, ObRob), Res, Geodes),
     state(NewNumMoves, robots(NewOrRob, ClRob, ObRob), res(NewOr, NewCl, NewOb), Geodes)) :-
   OrRob < MaxOrRob,
   NewOrRob is OrRob + 1,
   Res = res(Or, _, _),
   TimeNeeded is max(ceil((OrReq - Or) / OrRob), 0) + 1,
   NewNumMoves is NumMoves - TimeNeeded,
   NewNumMoves > 0,
   gather(TimeNeeded, robots(OrRob, ClRob, ObRob), Res, res(OrBC, NewCl, NewOb)),
   NewOr is OrBC - OrReq.

% For a given blueprint, determine the maximum number of robots of a particular
% type it makes sense to build, as there's no point collecting more resources
% than we can spend
max_robots(blueprint(OrReqOr, OrReqCl, (OrReqOb, ClReqOb), (OrReqGe, ObReqGe)), max_robots(MaxOr, ClReqOb, ObReqGe)) :-
   max_list([OrReqOr, OrReqCl, OrReqOb, OrReqGe], MaxOr).

% The start state is a single ore robot and no resources
start_state(Time, state(Time, robots(1, 0, 0), res(0, 0, 0), 0)).

% To search the graph (DFS) perform a transition and then move
% However, we keep track of the best results reached at the end of every path and
% then only try subsequent paths if it is possible we could do better
% Instead of using findall/3 just fail on every path because we add the best
% result to the dictionary; then collect the best result at the end using best/1
% and retract it
:- dynamic best/1.

search_(Blueprint, MaxRobots, State) :-
   move(Blueprint, MaxRobots, State, NewState),
   NewState = state(NumMoves, _, _, GeodesSoFar),
   add_geodes(NumMoves, GeodesPossible),
   TotalPossible is GeodesSoFar + GeodesPossible,
   best(BestSoFar),
   TotalPossible > BestSoFar,
   search_(Blueprint, MaxRobots, NewState).
search_(_, _, state(_, _, _, Geodes)) :-
   best(BestSoFar),
   Geodes > BestSoFar, !,
   retract(best(BestSoFar)),
   assertz(best(Geodes)),
   fail.

search(Blueprint, Time, _) :-
   assertz(best(0)),
   max_robots(Blueprint, MaxRobots),
   start_state(Time, StartState),
   search_(Blueprint, MaxRobots, StartState).
search(_, _, Geodes) :-
   best(Geodes),
   retract(best(Geodes)).

% Find the quality value for part 1
find_quality(Blueprints, Quality) :-
   nth1(Index, Blueprints, Blueprint),
   search(Blueprint, 24, Geodes),
   Quality is Geodes * Index.

% Find the sum of the quality values for part 1
quality_sum(Blueprints, QualitySum) :-
   findall(Quality, find_quality(Blueprints, Quality), QualityList),
   sum_list(QualityList, QualitySum), !.

% Find the quality multiplier for the first three blueprints for part 2
quality_mult([B1, B2, B3|_], QualityMult) :-
   maplist([Blueprint, Quality]>>search(Blueprint, 32, Quality), [B1, B2, B3], [Q1, Q2, Q3]),
   QualityMult is Q1 * Q2 * Q3.

:- phrase_from_file(blueprints(Blueprints), '../input/19.txt'),
   quality_sum(Blueprints, QualitySum),
   writeln(QualitySum),
   quality_mult(Blueprints, QualityMult),
   writeln(QualityMult).

:- begin_tests(aoc202219).

sample(Blueprints) :-
   Blueprints = [
      blueprint(4, 2, (3, 14), (2, 7)),
      blueprint(2, 3, (3, 8), (3, 12))
   ].

test(search) :-
   search(blueprint(4, 2, (3, 14), (2, 7)), 24, 9).

test(sample_part1) :-
   sample(Blueprints),
   quality_sum(Blueprints, 33).

test(sample_part2) :-
   sample([Blueprint1, Blueprint2]),
   search(Blueprint1, 32, 56),
   search(Blueprint2, 32, 62).

:- end_tests(aoc202219).

:- run_tests(aoc202219).

:- halt.
