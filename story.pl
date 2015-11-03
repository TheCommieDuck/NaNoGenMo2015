:- module(story, [
	goal/3, need/3, need_threshold/4,
	character/1, location/1,
	location/2, id_name/2, adjacent/3]).

:- use_module(utils).

:- dynamic character/1.
:- dynamic location/2.
:- dynamic location/1.
:- dynamic adjacent/3.
:- dynamic id_name/2.

:- dynamic need/3.
:- dynamic goal/3.

opposite_direction(west, east).
opposite_direction(north, south).
opposite_direction(north-west, south_east).
opposite_direction(north_east, south_west).
opposite_direction(X, Y) :-
	fact(opposite_direction(Y, X)).

adjacent(X, Y, Dir) :-
	opposite_direction(Dir, Dir2),
	fact(adjacent(Y, X, Dir2)).

need_threshold(hunger, 0.1, 0.5, 30).

goal(_, wait, -42).