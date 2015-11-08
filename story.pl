:- module(story, [
	character_goal/3, need/3, need_threshold/4,
	character/1, location/1, food/1, beverage/1,
	believes/2, known_about/3,
	current_time/1, 
	contains/2,
	story_precondition/1,
	hero/1, region/1, sibling/2, mother/2, father/2, family/2,
	house/1, village/1,
	encompassed_by/2, talked/3, initial_description/0,
	location/2, id_name/2, property_name/2, adjacent/3]).

:- use_module(utils).

:- dynamic character/1.
:- dynamic character/2.
:- dynamic location/2.
:- dynamic location/1.
:- dynamic adjacent/3.
:- dynamic id_name/2.

:- dynamic contains/2.

:- dynamic believes/3.
:- dynamic need/3.
:- dynamic character_goal/3.

:- dynamic sibling/2.
:- dynamic mother/2.
:- dynamic father/2.
:- dynamic family/2.

:- dynamic initial_description/0.

:- dynamic house/1.

:- dynamic encompassed_by/2.
:- dynamic talked/3.
:- dynamic house/1.
:- dynamic village/1.
:- dynamic story_precondition/1.
:- dynamic hero/1.
:- dynamic region/1.
:- dynamic food/1.
:- dynamic beverage/1.

:- dynamic known_about/3.

believes(Char, Thing) :-
	believes(Char, Thing, _).

current_time(1).

encompassed_by(X, Y) :-
	region(Y),
	location(X, Loc),
	encompassed_by(Loc, Y).

opposite_direction(west, east).
opposite_direction(north, south).
opposite_direction(north_west, south_east).
opposite_direction(north_east, south_west).
opposite_direction(X, Y) :-
	fact_story(opposite_direction(Y, X)).

adjacent(X, Dir, Y) :-
	opposite_direction(Dir, Dir2),
	fact_story(adjacent(Y, Dir2, X)).

need_threshold(hunger, 0.3, 0.5, 300).

character_goal(_, wait, -42).

property_name(location(X, Y), Name) :-
	id_name(X, XName),
	id_name(Y, YName),
	atomic_list_concat([XName, 'is located at the', YName], ' ', Name).

property_name(X, X).

character(X) :-
	character(X, alive).

dead(X) :-
	character(X, dead).