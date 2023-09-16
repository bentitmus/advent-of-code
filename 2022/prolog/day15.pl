% Advent of Code 2022 - Day 15
:- use_module(library(dcg/basics),     [eos//0, eol//0, integer//1]).
:- use_module(library(dcg/high_order), [sequence//2]).
:- use_module(library(pio),            [phrase_from_file/2]).
:- use_module(library(clpfd),          [fdset_interval/3, fdset_union/2, fdset_subtract/3, fdset_member/2, fdset_size/2]).

% DCG for input file to get a list of sensor and beacon pairs
data(SensorBeaconList) --> sequence(sensor_beacon, SensorBeaconList), eos.

sensor_beacon((Sensor, Beacon)) --> "Sensor at ", coord(Sensor), ": closest beacon is at ", coord(Beacon), eol.

coord((X, Y)) --> "x=", integer(X), ", y=", integer(Y).

% Calculate the Manhattan distance between two points
% Or what the possible X values are for a given point, distance, and Y value
manhattan_distance((X1, Y1), (X2, Y2), Distance) :-
   var(Distance), !,
   Distance is abs(X1 - X2) + abs(Y1 - Y2).
manhattan_distance((X1, Y1), (X2, Y2), Distance) :-
   nonvar(Distance), !,
   XDist is Distance - abs(Y1 - Y2),
   XDist >= 0,
   FirstX is X1 - XDist, SecondX is X1 + XDist,
   (
      FirstX \= SecondX
   -> (X2 = FirstX ; X2 = SecondX)
   ;  X2 = FirstX
   ).

% Find the range for a given sensor-beacon on a particular row
find_range_for_sensor_beacon(YPos, ((SX, SY), (BX, BY)), (XStart, XEnd)) :-
   manhattan_distance((SX, SY), (BX, BY), Distance),
   manhattan_distance((SX, SY), (XStart, YPos), Distance),
   (
      manhattan_distance((SX, SY), (XEnd, YPos), Distance),
      XStart \= XEnd
   -> true
   ;  XEnd = XStart
   ), !.

% Remove the beacon from the range if it is in the range
% This has a bug because it doesn't consider the Y position, so if the beacon
% happens to be the opposite side (y-direction) to the current row then this
% won't give the correct answer
adjust_range((X, X), X, (_, _)) :- !, fail.
adjust_range((XStart, XEnd), XStart, (NewXStart, XEnd)) :- !, NewXStart is XStart + 1.
adjust_range((XStart, XEnd), XEnd, (XStart, NewXEnd)) :- !, NewXEnd is XEnd - 1.
adjust_range(Pos, _, Pos).

% Get the range for a seansor-beacon in a given row
get_range(YPos, SensorBeacon, Range) :-
   find_range_for_sensor_beacon(YPos, SensorBeacon, UnadjustedRange),
   SensorBeacon = (_, (BX, _)),
   adjust_range(UnadjustedRange, BX, Range).

% Count the number of positions there cannot be a beacon in the current row
count_no_beacon(YPos, SensorBeaconList, Number) :-
   convlist(get_range(YPos), SensorBeaconList, RangeList),
   maplist([(XStart, XEnd), Set]>>fdset_interval(Set, XStart, XEnd), RangeList, Sets),
   fdset_union(Sets, Union),
   fdset_size(Union, Number).

% Find an x value that would not be detected by any sensors in a given row,
% within a given range (from 0 to MaxRange)
find_x(Y, MaxRange, SensorBeaconList, X) :-
   convlist(find_range_for_sensor_beacon(Y), SensorBeaconList, RangeList),
   maplist([(XStart, XEnd), Set]>>fdset_interval(Set, XStart, XEnd), RangeList, Sets),
   fdset_union(Sets, Union),
   fdset_interval(ValidRange, 0, MaxRange),
   fdset_subtract(ValidRange, Union, Difference), !,
   fdset_member(X, Difference).

% Find an (x, y) point which would not be detected by any sensors within a given
% range (from 0 to MaxRange)
find_x_y(Y, MaxRange, _, _) :- Y > MaxRange, !, fail.
find_x_y(Y, MaxRange, SensorBeaconList, (X, Y)) :-
   find_x(Y, MaxRange, SensorBeaconList, X), !.
find_x_y(Y, MaxRange, SensorBeaconList, (X, FinalY)) :-
   NewY is Y + 1,
   find_x_y(NewY, MaxRange, SensorBeaconList, (X, FinalY)).
find_x_y(SensorBeaconList, MaxRange, (X, Y)) :-
   find_x_y(0, MaxRange, SensorBeaconList, (X, Y)).

% Compute the tuning frequency for the point not detected by any sensors within
% a given range (0 to MaxRange)
find_tuning_freq(MaxRange, SensorBeaconList, TuningFreq) :-
   find_x_y(SensorBeaconList, MaxRange, (X, Y)),
   TuningFreq is (4000000 * X) + Y.

:- phrase_from_file(data(SensorBeaconList), '2022/input/day15.txt'),
   count_no_beacon(2000000, SensorBeaconList, NumberNoBeacon),
   writeln(NumberNoBeacon),
   find_tuning_freq(4000000, SensorBeaconList, TuningFreq),
   writeln(TuningFreq).

:- begin_tests(aoc202215).

sample(SensorBeaconList) :-
   SensorBeaconList = [
      (( 2, 18), (-2, 15)),
      (( 9, 16), (10, 16)),
      ((13,  2), (15,  3)),
      ((12, 14), (10, 16)),
      ((10, 20), (10, 16)),
      ((14, 17), (10, 16)),
      (( 8,  7), ( 2, 10)),
      (( 2,  0), ( 2, 10)),
      (( 0, 11), ( 2, 10)),
      ((20, 14), (25, 17)),
      ((17, 20), (21, 22)),
      ((16,  7), (15,  3)),
      ((14,  3), (15,  3)),
      ((20,  1), (15,  3))
   ].

test(manhattan_distance) :-
   manhattan_distance(( 2, 18), (-2, 15), 7),
   manhattan_distance((20, 14), (25, 17), 8),
   manhattan_distance((14,  3), (15,  3), 1), !.

test(find_range_for_sensor_beacon) :-
   find_range_for_sensor_beacon(10, ((8, 7), (2, 10)), (2, 14)),
   adjust_range((2, 14), 2, (3, 14)).

test(get_range) :-
   get_range(10, ((8, 7), (2, 10)), (3, 14)).

test(count_no_beacon) :-
   sample(SensorBeaconList),
   count_no_beacon(10, SensorBeaconList, 26).

test(find_tuning_freq) :-
   sample(SensorBeaconList),
   find_tuning_freq(20, SensorBeaconList, 56000011).

:- end_tests(aoc202215).

:- run_tests(aoc202215).

:- halt.
