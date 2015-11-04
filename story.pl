:- module(story, [
	character_goal/3, need/3, need_threshold/4,
	character/1, location/1, food/1, beverage/1,
	believes/2, known_about/3,
	current_time/1, 
	location/2, id_name/2, property_name/2, adjacent/3]).

:- use_module(utils).

:- dynamic character/1.
:- dynamic location/2.
:- dynamic location/1.
:- dynamic adjacent/3.
:- dynamic id_name/2.

:- dynamic believes/3.
:- dynamic need/3.
:- dynamic character_goal/3.

:- dynamic food/1.
:- dynamic beverage/1.

:- dynamic known_about/3.

believes(Char, Thing) :-
	believes(Char, Thing, _).

current_time(1).

opposite_direction(west, east).
opposite_direction(north, south).
opposite_direction(north-west, south_east).
opposite_direction(north_east, south_west).
opposite_direction(X, Y) :-
	fact(opposite_direction(Y, X)).

adjacent(X, Y, Dir) :-
	opposite_direction(Dir, Dir2),
	fact(adjacent(Y, X, Dir2)).

need_threshold(hunger, 0.3, 0.5, 300).

character_goal(_, wait, -42).

property_name(location(X, Y), Name) :-
	id_name(X, XName),
	id_name(Y, YName),
	atomic_list_concat([XName, 'is located at the', YName], ' ', Name).

property_name(X, X).