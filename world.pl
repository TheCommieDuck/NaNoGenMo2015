:- module(world, [create_world/1, create_hero/2, init_story_creation/0]).

:- use_module(story).

goal(location(dave, outside_house)).

move(Char, Loc) :-
	character(Char),
	location(Char, OldLoc),
	adjacent(Loc, OldLoc),
	retract(location(Char, OldLoc)),
	assert(location(Char, Loc)).

adjacent(X, Y) :-
	fact(adjacent(Y, X)).

fact(Fact):-
	clause(Fact, true).

init_story_creation :-
	nb_setval(story_refs, []).

step_forward :-
	findall(X, character(X), Characters),
	step_forward(Characters).

step_forward([]).

step_forward([C|Cs]) :-
	(step(C), ! ; format('An error occured with stepping for ~s', [C])),
	step_forward(Cs).

step(Char) :-
	id_name(Char, Name),
	location(Char, Loc),
	id_name(Loc, LocName),
	format('~s is in the ~s and has done nothing.', [Name, LocName]).

cleanup_ref(Refs):-
	forall(member(X, Refs), erase(X)).

assert_story(Fact) :-
	assert(:(story, Fact), Ref),
	nb_getval(story_refs, Refs),
	nb_setval(story_refs, [Ref|Refs]).

% the world has at least 1 of the following:
% - a major city; the ruler(s) live here.
% - a wilderness (forest, etc)
% - a small town/village. rural.
% - potentially some more, but guaranteed one of each of the above.
create_world(Start) :-
	%the world is set in 1 region. It'll be a single world. Each major city is a separate state.
	random_line_from_file('tac.csv', 48424, String),
	atomic_list_concat([CityName, County], ',', String),
	name_to_id(County, CountyID),
	assert_story(region(CountyID)),
	assert_story(id_name(CountyID, County)),
	create_city(CityName, CountyID, Start).
	%create_wilderness(CountyID).
	%create_town,
	%Extra is random(4) + 3.
	%create_extra(Extra),

% a city currently consists of something similar to the world gen - it's guaranteed 1 of things, then optionally some other things.

create_city(CityName, County, CityGateID) :-
	name_to_id(CityName, CityID),
	assert_story(contains(County, CityName)),
	assert_story(region(CityID)),
	assert_story(id_name(CityID, CityName)),
	create_local_location(city_gate, CityID, CityGateID),
	create_local_location(outside_city_gate, CityID, OutsideCityID),
	assert_story(location(CityGateID)),
	assert_story(contains(CityID, CityGateID)),
	atomic_list_concat([CityName, 'city gate'], ' ', CityGateName),
	assert_story(id_name(CityGateID, CityGateName)),
	assert_story(location(OutsideCityID)),
	assert_story(contains(CityID, OutsideCityID)),
	assert_story(id_name(OutsideCityID, 'outside the city gate')),
	assert_story(adjacent(OutsideCityID, CityGateID)).

create_local_location(SubID, ContainID, LocalID) :-
	atomic_list_concat([ContainID, SubID], '_', LocalID).
	
name_to_id(Name, ID) :-
	downcase_atom(Name, LowName),
	atomic_list_concat(L, ' ', LowName),
	atomic_list_concat(L, '_', ID).

create_hero(Hero, Start) :-
	Hero = dave,
	assert_story(character(Hero)),
	assert_story(hero(Hero)),
	assert_story(id_name(Hero, 'Dave')),
	assert(location(Hero, Start)).

random_line_from_file(File, Lines, Atom) :-
	open(File, read, Stream),
	Line is random(Lines),
	read_line_to_atom(Stream, Line, Atom),
	close(Stream).

read_line_to_atom(Stream, Line, Atom) :-
	Line =< 1,
	read_line_to_string(Stream, String),  !,
	atom_string(Atom, String).

read_line_to_atom(Stream, Line, String) :-
	Line2 is Line-1,
	read_line_to_codes(Stream, _),
	read_line_to_atom(Stream, Line2, String).
