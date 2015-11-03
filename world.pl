:- module(world, [create_world/1, create_hero/2, init_story_creation/0]).

:- use_module(utils).
:- use_module(story).


%% Creation/Generation Predicates
init_story_creation :-
	nb_setval(story_refs, []).

assert_story(Fact) :-
	assert(:(story, Fact), Ref),
	nb_getval(story_refs, Refs),
	nb_setval(story_refs, [Ref|Refs]).

create_id_name(ID, Name) :-
	generate_id(Name, ID),
	assert_story(id_name(ID, Name)).

create_region(RegionName, RegionID) :-
	create_id_name(RegionID, RegionName),
	assert_story(region(RegionID)).

create_local_location(Name, ContainID, NameID) :-
	create_id_name(NameID, Name),
	assert_story(contains(ContainID, NameID)),
	assert_story(location(NameID)).

% the world has at least 1 of the following:
% - a major city; the ruler(s) live here.
% - a wilderness (forest, etc)
% - a small town/village. rural.
% - potentially some more, but guaranteed one of each of the above.
create_world(Start) :-
	%the world is set in 1 region. It'll be a single world. Each major city is a separate state.
	random_line_from_file('tac.csv', 48424, ',', [CityName, County]),
	create_region(County, CountyID),
	create_city(CityName, CountyID, Start).

% a city currently consists of something similar to the world gen - it's guaranteed 1 of things, then optionally some other things.

create_city(CityName, CountyID, CityGateID) :-
	create_region(CityName, CityID),
	assert_story(contains(CountyID, CityID)),
	create_local_location('city gate', CityID, CityGateID),
	create_local_location('outside the city gate', CityID, OutsideCityID),
	assert_story(adjacent(OutsideCityID, CityGateID, west)).

create_hero(Hero, Start) :-
	Hero = bob,
	assert_story(hero(Hero)),
	assert_story(character(Hero)),
	assert_story(id_name(alice, 'Alice')),
	assert_story(character(alice)),
	assert_story(id_name(Hero, 'Bob')),
	assert_story(location(Hero, Start)),
	assert_story(location(alice, Start)).

move(Char, Loc) :-
	character(Char),
	location(Char, OldLoc),
	adjacent(Loc, OldLoc, _),
	retract(location(Char, OldLoc)),
	assert(location(Char, Loc)).



step_forward :-
	findall(X, character(X), Characters),
	step_forward(Characters).

step_forward([]).

step_forward([C|Cs]) :-
	(step(C), ! ; write_debug_message('An error occured with stepping for ~s', [C])),
	step_forward(Cs).

step(Char) :-
	id_name(Char, Name),
	location(Char, Loc),
	id_name(Loc, LocName),
	format('~s is in the ~s and has done nothing.', [Name, LocName]), nl.