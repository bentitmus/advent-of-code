% Advent of Code 2022 - Day 16
:- use_module(library(dcg/basics),     [eos//0, eol//0, integer//1, nonblanks//1]).
:- use_module(library(dcg/high_order), [sequence//2]).
:- use_module(library(pio),            [phrase_from_file/2]).

% DCG for input file to give a list of valves
valves(Valves) --> sequence(valve, Valves), eos.

valve(valve(ValveName, Pressure, Connected)) --> "Valve ", name(ValveName), " has flow rate=", integer(Pressure), "; ", ("tunnel leads" | "tunnels lead"), (" to valve " | " to valves "), list_of_valves(Connected).

list_of_valves([Valve])        --> name(Valve), eol.
list_of_valves([Valve|Valves]) --> name(Valve), ", ", list_of_valves(Valves).

name(Name) --> [Code1, Code2], {string_codes(Name, [Code1, Code2])}.

% List of valves with a non-zero flow and the start valve
valves_of_interest(Valves, ValvesOfInterest) :-
   include(\=(valve(_, 0, _)), Valves, NonZeroValves),
   maplist([valve(Name, _, _), Name]>>true, [valve("AA", _, _)|NonZeroValves], ValvesOfInterest).

% Find the costs of moving from one valve to another
bfs_costs([], [], []) :- !.
bfs_costs([explore(Valve, Cost, ConnectedValves)|ExploreList], UnexploredValves, [cost(Valve, Cost)|Costs]) :-
   NextCost is Cost + 1,
   partition([valve(NextValve, _, _)]>>member(NextValve, ConnectedValves), UnexploredValves, FurtherValvesToExplore, NewUnexploredValves),
   maplist({NextCost}/[valve(NextValve, _, NextConnectedValves), explore(NextValve, NextCost, NextConnectedValves)]>>true, FurtherValvesToExplore, FurtherExploreList),
   append(ExploreList, FurtherExploreList, NewExploreList),
   bfs_costs(NewExploreList, NewUnexploredValves, Costs).

% Find the costs of moving to all other valves from a given starting valve
costs_from_start(StartValve, Valves, Costs) :-
   select(valve(StartValve, _, ConnectedValves), Valves, UnexploredValves),
   bfs_costs([explore(StartValve, 0, ConnectedValves)], UnexploredValves, [_|Costs]), !.

% Only succeed for a valve of interest, in which case add one to the cost to indicate
% that the valve is opened when moving there
update_cost(ValvesOfInterest, cost(Name, Cost), cost(Name, NewCost)) :-
   member(Name, ValvesOfInterest),
   NewCost is Cost + 1.

% Find all costs to valves of interest from a given starting valve
filter_costs_from_start(StartValve, Valves, ValvesOfInterest, Costs) :-
   costs_from_start(StartValve, Valves, AllCosts),
   convlist(update_cost(ValvesOfInterest), AllCosts, Costs).

% Find the graph connecting all valves of interest, counting the start valve but only
% as an exit node in the graph
% Costs assume that when moving to a valve it is opened
graph_for_valves_of_interest(Valves, [StartValve|ValvesOfInterest], Graph) :-
   maplist({Valves, ValvesOfInterest}/[Valve, Valve-Costs]>>filter_costs_from_start(Valve, Valves, ValvesOfInterest, Costs), [StartValve|ValvesOfInterest], Graph).

% Find the pressures just for the valves of interest (not including the starting node)
pressures_for_valves_of_interest(Valves, [_|ValvesOfInterest], Pressures) :-
   maplist({Valves}/[Valve, pressure(Valve, Pressure)]>>member(valve(Valve, Pressure, _), Valves), ValvesOfInterest, Pressures), !.

% Find the valves of interest, graph, and pressures from a given list of nodes
prepare_values(Valves, ValvesOfInterest, Graph, Pressures) :-
   valves_of_interest(Valves, [StartValve|ValvesOfInterest]),
   graph_for_valves_of_interest(Valves, [StartValve|ValvesOfInterest], Graph),
   pressures_for_valves_of_interest(Valves, [StartValve|ValvesOfInterest], Pressures).

:- dynamic found/2.

% Update the best found value if a newer, better, one is found for a given route
% This assumes the route is already sorted
replace_if_better(Route, Value) :-
   found(Route, OldValue),
   OldValue < Value, !,
   retract(found(Route, OldValue)),
   assertz(found(Route, Value)).
replace_if_better(Route, _) :-
   found(Route, _), !.
replace_if_better(Route, Value) :-
   assertz(found(Route, Value)).

% Find a path through the graph opening a selection of valves once only
% Updates the best found/2 value in the dictionary with the best value found
% Always fails after all paths are found
find_pressure(RemainingCost, StartValve, Graph, Pressures, Accumulator, UnexploredValves, Route) :-
   select(ConnectedValve, UnexploredValves, NewUnexploredValves),
   member(StartValve-ConnectedValves, Graph),
   member(cost(ConnectedValve, Cost), ConnectedValves),
   NewRemainingCost is RemainingCost - Cost,
   NewRemainingCost > 0,
   member(pressure(ConnectedValve, Pressure), Pressures),
   NewAccumulator is Accumulator + (Pressure * NewRemainingCost),
   find_pressure(NewRemainingCost, ConnectedValve, Graph, Pressures, NewAccumulator, NewUnexploredValves, [ConnectedValve|Route]).
find_pressure(_, _, _, _, Accumulator, _, Route) :-
   sort(Route, SortedRoute),
   replace_if_better(SortedRoute, Accumulator),
   fail.

% Find all paths through the graph and return the best pressure value for each
% route in a list of route-pressure values
find_pressure(Cost, ValvesOfInterest, Graph, Pressures, _) :-
   find_pressure(Cost, "AA", Graph, Pressures, 0, ValvesOfInterest, []).
find_pressure(_, _, _, _, Results) :-
   findall(SortedRoute-Pressure, found(SortedRoute, Pressure), Results),
   retractall(found(_, _)).

% Finds the highest pressure that can be obtained with 30 minutes
find_most_pressure(Valves, MostPressure) :-
   prepare_values(Valves, ValvesOfInterest, Graph, Pressures), !,
   find_pressure(30, ValvesOfInterest, Graph, Pressures, Results),
   maplist([_-Pressure, Pressure]>>true, Results, FoundPressures),
   max_list(FoundPressures, MostPressure), !.

% Finds a non-intersecting pair of routes and returns the combined pressure
find_pair(AllPressures, Value) :-
   member(FirstRoute-FirstPressure, AllPressures),
   member(SecondRoute-SecondPressure, AllPressures),
   ord_intersect(FirstRoute, SecondRoute, []),
   Value is FirstPressure + SecondPressure.

% Finds the highest pressure that can be obtained in 20 minutes with two people
find_most_with_elephant(Valves, MostPressure) :-
   prepare_values(Valves, ValvesOfInterest, Graph, Pressures), !,
   find_pressure(26, ValvesOfInterest, Graph, Pressures, AllPressures),
   findall(Value, find_pair(AllPressures, Value), AllValues),
   max_list(AllValues, MostPressure).

:- phrase_from_file(valves(Valves), '../input/day16.txt'),
   find_most_pressure(Valves, MostPressure),
   writeln(MostPressure),
   find_most_with_elephant(Valves, MostPressureWithElephant),
   writeln(MostPressureWithElephant).

:- begin_tests(aoc202216).

sample(Valves) :-
   Valves = [
     valve("AA",  0, ["DD", "II", "BB"]),
     valve("BB", 13, ["CC", "AA"]),
     valve("CC",  2, ["DD", "BB"]),
     valve("DD", 20, ["CC", "AA", "EE"]),
     valve("EE",  3, ["FF", "DD"]),
     valve("FF",  0, ["EE", "GG"]),
     valve("GG",  0, ["FF", "HH"]),
     valve("HH", 22, ["GG"]),
     valve("II",  0, ["AA", "JJ"]),
     valve("JJ", 21, ["II"])
   ].

test(valves_of_interest) :-
   sample(Valves),
   valves_of_interest(Valves, ["AA", "BB", "CC", "DD", "EE", "HH", "JJ"]).

test(costs_from_start) :-
   sample(Valves),
   costs_from_start("AA", Valves, [cost("BB",1),cost("DD",1),cost("II",1),cost("CC",2),cost("EE",2),cost("JJ",2),cost("FF",3),cost("GG",4),cost("HH",5)]).

test(filter_costs_from_start) :-
   sample(Valves),
   filter_costs_from_start("AA", Valves, ["AA", "BB", "HH"], [cost("BB", 2), cost("HH", 6)]).

test(graph_for_valves_of_interest) :-
   sample(Valves),
   graph_for_valves_of_interest(Valves, ["AA", "BB", "HH"], ["AA"-[cost("BB", 2), cost("HH", 6)], "BB"-[cost("HH", 7)], "HH"-[cost("BB", 7)]]).

test(pressures_for_valves_of_interest) :-
   sample(Valves),
   pressures_for_valves_of_interest(Valves, ["AA", "BB", "HH"], [pressure("BB", 13), pressure("HH", 22)]).

test(find_most_pressure) :-
   sample(Valves),
   find_most_pressure(Valves, 1651).

test(find_most_with_elephant) :-
   sample(Valves),
   find_most_with_elephant(Valves, 1707).

:- end_tests(aoc202216).

:- run_tests(aoc202216).

:- halt.
