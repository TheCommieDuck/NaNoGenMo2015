:- module(world, [create_world/1, create_hero/2, init_story_creation/0]).

:- use_module(utils).
:- use_module(story).


%% Creation/Generation Predicates
init_story_creation.

assert_story(Fact) :-
	assert(:(story, Fact)).

retract_story(Fact) :-
	retract(:(story, Fact)).

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

create_person(Name, ID) :-
	create_id_name(ID, Name),
	assert_story(character(ID)),
	assert_story(person(ID)),
	assert_story(need(ID, hunger, 0)).

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

create_random_food(FoodID, LocID) :-
	random_line_from_file('food.txt', 38358, '\t', [_, _, Name]),
	create_id_name(FoodID, Name),
	assert_story(food(FoodID)),
	assert_story(location(FoodID, LocID)).

create_city(CityName, CountyID, CityGateID) :-
	create_region(CityName, CityID),
	assert_story(contains(CountyID, CityID)),
	create_local_location('city gate', CityID, CityGateID),
	create_random_food(FoodID, CityGateID),
	create_local_location('outside the city gate', CityID, OutsideCityID),
	assert_story(adjacent(OutsideCityID, CityGateID, west)).

create_hero(Hero, Start) :-
	Hero = bob,
	create_person('Bob', BobID),
	assert_story(hero(BobID)),
	create_person('Alice', AliceID),
	assert_story(location(BobID, Start)),
	assert_story(location(AliceID, Start)).

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
	(step(C), ! ; write_debug_message('An error occured with stepping for %w', [C])),
	step_forward(Cs).

step(Char) :-
	update_needs(Char),
	do_goal(Char).
	%perform_action(Char, Goal),
	%format('~s is in the ~s and has done nothing.', [Name, LocName]), nl.

%%% Goals

do_goal(Char) :-
	pick_goal(Char, goal(Char, Action, Priority)),
	do_action(Char, Action).

update_needs(Char) :-
	forall(need(Char, Need, Amount), update_need(Char, Need, Amount)).

update_need(_, Need, _) :-
	need_threshold(Need, 0, _, _).

update_need(Char, Need, Amount) :-
	need_threshold(Need, Incr, Threshold, Priority),
	New_Amount is Amount + Incr,
	retract_story(need(Char, Need, _)),
	assert_story(need(Char, Need, New_Amount)),
	(
		Threshold < New_Amount,
		\+ goal(Char, Need, _),
		create_goal(Char, Need, Priority),
		id_name(Char, Name),
		write_debug_message('%w has become %w', [Name, Need])
		;
		true
	).

create_goal(Char, Need, Priority) :-
	write_debug_message('Added a new goal: %w is now trying to %w', [Char, Need]),
	assert_story(goal(Char, Need, Priority)).

pick_goal(Char, Goal) :-
	findall(Priority, goal(Char, _, Priority), Goals),
	max_list(Goals, Best),
	findall(X, goal(Char, X, Best), BestGoals),
	random_member(BestGoal, BestGoals),
	Goal = goal(Char, BestGoal, Best).

% Actions
do_action(Char, hunger) :-
	goal(Char, hunger, Priority),
	find_item_type(Char, food, Priority),
	do_goal(Char).

do_action(Char, thirst) :-
	goal(Char, hunger, Priority),
	find_item_type(Char, beverage, Priority),
	do_goal(Char).

do_action(Char, X) :-
	id_name(Char, Name),
	write_debug_message('%w is doing %w', [Name, X]).

% to find an item:
% if the character recently thinks the item is in their location, go and pick it up. 
% Action: pick up item.
% if the character does not know if the item is nearby, then look.
% Action: look around.
% if the character can see another character, ask them.
% Action: talk.
% if the character thinks there is an item of that type SOMEWHERE, pick the closest one and start going there, checking along the way.
% Action: travel.
% if the character has no idea, then attempt to find someone and ask.
% Action: explore.
find_item_type(Char, Type, Priority) :-
	location(Char, Location),
	believes(Char, location(Item, Location)),
	call(Type, Item),
	Priority2 is Priority+1,
	create_goal(Char, pick_up, Priority2).

find_item_type(Char, Type, Priority) :-
	location(Char, Location),
	\+ (believes(Char, location(Item, Location)), call(Type, Item)),
	Priority2 is Priority+1,
	create_goal(Char, look, Priority2).

% characters can always see other characters in the same place.
find_item_type(Char, Type, Priority) :-
	location(Char, Location),
	location(Char2, Location),
	Char \= Char2,
	create_goal(Char, talk(Char2), Priority2).

find_item_type(Char, Type, Priority) :-
	location(Char, Location),
	believes(Char, location(Item, Somewhere)),
	food(Item),
	create_goal(Char, travel(Somewhere), Priority2).

find_item_type(Char, Type, Priority) :-
	location(Char, Location),
	\+ (believes(Char, location(Item, _)), call(Type, Item)),
	create_goal(Char, explore, Priority2).