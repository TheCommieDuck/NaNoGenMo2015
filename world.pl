:- module(world, [create_world/1, populate_world/0, init_story_creation/0, add_story_precondition/1, continue_plot/1]).

:- use_module(utils).
:- use_module(story).


%% Creation/Generation Predicates
init_story_creation.

add_story_precondition(Cond) :-
	assert_story(story_precondition(Cond)).

assert_story(Fact) :-
	assert(:(story, Fact)).

broadcast_assert_story(Fact, Loc) :-
	assert_story(Fact),
	forall((location(X, Loc), character(X)), now_believes(X, Fact)).

broadcast_retract_story(Fact, Loc) :-
	retract_story(Fact),
	forall((location(X, Loc), character(X)), retract_all_beliefs(X, Fact)).

retract_story(Fact) :-
	retract(:(story, Fact)), !
	;
	true.

call_story(Thing, Thing2) :-
	call(:(story, Thing), Thing2).

create_id_name(ID, Name) :-
	generate_id(Name, ID),
	assert_story(id_name(ID, Name)).

create_region(RegionName, RegionID) :-
	create_id_name(RegionID, RegionName),
	assert_story(region(RegionID)).

create_local_location(Name, ContainID, NameID) :-
	create_id_name(NameID, Name),
	assert_story(encompassed_by(NameID, ContainID)),
	assert_story(location(NameID)).

create_person(Name, ID) :-
	create_person(Name, ID, alive).

create_person(Name, ID, Alive) :-
	create_id_name(ID, Name),
	assert_story(character(ID, Alive)),
	assert_story(person(ID)),
	(
		Alive = alive,
		create_needs(ID)
		;
		true
	).

create_needs(ID) :-
	assert_story(need(ID, hunger, 0)).

% the world is currently pregenned:
% see this MSPaint drawing: http://i.imgur.com/biyGMVB.png
create_world(_) :-
	random_line_from_file('tac.csv', 48424, ',', [CityName, County]),
	random_line_from_file('tac.csv', 48424, ',', [VillageName, _]),
	create_region(County, CountyID),
	create_city(CityName, C1, CountyID),
	create_village(VillageName, V1, CountyID),
	assert_story(adjacent(V1, west, C1)).


% a city currently consists of something similar to the world gen - it's guaranteed 1 of things, then optionally some other things.

create_random_food(FoodID, LocID) :-
	random_line_from_file('food.txt', 38358, '\t', [_, _, Name]),
	create_id_name(FoodID, Name),
	assert_story(food(FoodID)),
	assert_story(location(FoodID, LocID)).

create_castle(Place, _, ID) :-
	create_local_location('castle', Place, ID).

create_house(Place, _, ID) :-
	create_local_location('house', Place, ID),
	assert_story(house(ID)).

create_village(VillageName, VillageID, CountyID) :-
	create_region(VillageName, VillageID),
	assert_story(village(VillageID)),
	assert_story(encompassed_by(VillageID, CountyID)),
	create_local_location('village square', VillageID, VSID),
	create_house(VillageID, east, H1),
	assert_story(adjacent(H1, west, VSID)),
	create_house(VillageID, west, H2),
	assert_story(adjacent(H2, east, VSID)).


create_city(CityName, OutsideCityID, CountyID) :-
	create_region(CityName, CityID),
	assert_story(encompassed_by(CityID, CountyID)),
	create_local_location('city gate', CityID, CityGateID),
	create_local_location('outside the city gate', CityID, OutsideCityID),
	assert_story(adjacent(OutsideCityID, west, CityGateID)),
	create_local_location('city square', CityID, CitySquareID),
	assert_story(adjacent(CityGateID, west, CitySquareID)),
	create_castle(CityID, west, CastleID),
	assert_story(adjacent(CitySquareID, west, CastleID)),

	create_local_location('north district', CityID, NDID),
	assert_story(adjacent(NDID, north, CitySquareID)),
	create_house(CityID, east, H1),
	assert_story(adjacent(H1, west, NDID)),
	create_house(CityID, west, H2),
	assert_story(adjacent(H2, east, NDID)),
	create_house(CityID, north_east, H3),
	assert_story(adjacent(H3, south_west, NDID)),
	create_house(CityID, north_west, H4),
	assert_story(adjacent(H4, south_east, NDID)),

	create_local_location('south district', CityID, SDID),
	assert_story(adjacent(SDID, south, CitySquareID)),
	create_house(CityID, east, H5),
	assert_story(adjacent(H5, west, SDID)),
	create_house(CityID, north_east, H6),
	assert_story(adjacent(H6, south_west, SDID)),
	create_house(CityID, south_west, H7),
	assert_story(adjacent(H7, north_east, SDID)).


assign_home(OwnerFam, Home) :-
	assert_story(owner(OwnerFam, Home)),
	forall((family(OwnerFam, People), member(X, People)), (assert_story(location(X, Home)))).

populate_world :-
	findall(H, house(H), Houses),
	create_person('Bob', BobID),
	assert_story(hero(BobID)),
	create_family(BobID, _, _, 2, FamID),
	member(HeroHouse, Houses),
	encompassed_by(HeroHouse, Loc),
	village(Loc),
	assign_home(FamID, HeroHouse),
	delete(Houses, HeroHouse, HousesLeft),
	populate_world(HousesLeft).

populate_world(_).

create_family(Char, MID, FID, SiblingNo, FamID) :-
	create_person('Mother', MID),
	create_person('Father', FID),
	assert_story(mother(Char, MID)),
	assert_story(father(Char, FID)),
	create_siblings(Char, MID, FID, SiblingNo, Siblings),
	generate_id('family', FamID),
	assert_story(family(FamID, [MID, FID, Char|Siblings])).

create_siblings(_, _, _, 0, []).
create_siblings(Char, MID, FID, SiblingNo, [SID|Sibs]) :-
	create_person('Sibling', SID),
	assert_story(sibling(Char, SID)),
	assert_story(mother(SID, MID)),
	assert_story(father(SID, FID)),
	Sib2 is SiblingNo-1,
	create_siblings(Char, MID, FID, Sib2, Sibs).

%% stepping


continue_plot(In) :-
	step_forward,
	write_debug_message('Do you wish to continue? (y/n)', []),
	read(In),
	(
		In = n, !
		;
		In = y,
		story_precondition(_), !,
		continue_plot(In)
		;
		true
	).

step_forward :-
	findall(P, story_precondition(P), Conds),
	check_preconditions(Conds),
	findall(X, character(X), Characters),
	step_forward(Characters).

check_preconditions([]).

check_preconditions([C|Cs]) :-
	(
		\+ C, !
		;
		C,
		retract_story(C),
		write_debug_message('Story condition %w has been satisfied', [C])
	),
	check_preconditions(Cs).

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
	pick_goal(Char, character_goal(Char, Action, _)),
	do_action(Char, Action).

complete_goal(Char, Action) :-
	id_name(Char, Name),
	write_debug_message('%w has completed the goal to %w.', [Name, Action]),
	retract_story(character_goal(Char, Action, _)).

set_need(Char, Need, Amount) :-
	retract_story(need(Char, Need, _)),
	assert_story(need(Char, Need, Amount)),
	check_need_threshold(Char, Need).

update_needs(Char) :-
	forall(need(Char, Need, Amount), update_need(Char, Need, Amount)).

update_need(_, Need, _) :-
	need_threshold(Need, 0, _, _).

update_need(Char, Need, Amount) :-
	need_threshold(Need, Incr, _, _),
	New_Amount is Amount + Incr,
	retract_story(need(Char, Need, _)),
	assert_story(need(Char, Need, New_Amount)),
	check_need_threshold(Char, Need).

check_need_threshold(Char, Need) :-
	need_threshold(Need, _, Threshold, Priority),
	id_name(Char, Name),
	need(Char, Need, Amount),
	(
		Amount >= Threshold,
		\+ character_goal(Char, Need, _),
		create_goal(Char, Need, Priority),
		id_name(Char, Name),
		write_debug_message('%w has become %w', [Name, Need])
		;
		Amount < Threshold,
		character_goal(Char, Need, _),
		complete_goal(Char, Need),
		write_debug_message('%w is no longer %w', [Name, Need])
	).

check_need_threshold(_, _).

create_goal(Char, Need, Priority) :-
	id_name(Char, Name),
	write_debug_message('Added a new goal: %w is now trying to %w', [Name, Need]),
	assert_story(character_goal(Char, Need, Priority)).

pick_goal(Char, Goal) :-
	findall(Priority, character_goal(Char, _, Priority), Goals),
	max_list(Goals, Best),
	findall(X, character_goal(Char, X, Best), BestGoals),
	random_member(BestGoal, BestGoals),
	Goal = character_goal(Char, BestGoal, Best).

% beliefs
now_believes(Char, Thing) :-
	current_time(Time),
	id_name(Char, Name),
	property_name(Thing, TName),
	write_debug_message('%w now believes that %w', [Name, TName]),
	now_believes(Char, Thing, Time).

now_believes(Char, location(X, Y), Time) :-
	retract_all_beliefs(Char, location(X, _)),
	assert_story(believes(Char, location(X, Y), Time)).

retract_all_beliefs(Char, Belief) :-
	retractall(:(story, believes(Char, Belief, _)))
	;
	true.

known_about_recently(Char, Location) :-
	known_about(Char, Location, Time),
	current_time(CTime),
	Diff is CTime - Time, !,
	Diff < 5.

talked_recently(Char, Char2) :-
	talked(Char, Char2, Time),
	current_time(CTime),
	Diff is CTime - Time, !,
	Diff < 5.

% Actions
do_action(Char, eat(Item)) :-
	id_name(Char, CName),
	id_name(Item, IName),
	location(Char, Loc),
	write_debug_message('%w eats the %w.', [CName, IName]),
	broadcast_retract_story(location(Item, _), Loc),
	complete_goal(Char, eat(Item)),
	set_need(Char, hunger, 0).

do_action(Char, hunger) :-
	character_goal(Char, hunger, Priority),
	(
		has_item(Char, food, Item),
		Priority2 is Priority+2,
		create_goal(Char, eat(Item), Priority2)
		;
		find_item_type(Char, food, Priority)
	),
	do_goal(Char).

do_action(Char, thirst) :-
	character_goal(Char, hunger, Priority),
	find_item_type(Char, beverage, Priority),
	do_goal(Char).

do_action(Char, look) :-
	location(Char, Loc),
	findall(X, location(X, Loc), Things),
	forall(member(Thing, Things), now_believes(Char, location(Thing, Loc))),
	current_time(Time),
	retract_story(known_about(Char, Loc, _)),
	assert_story(known_about(Char, Loc, Time)),
	complete_goal(Char, look).

do_action(Char, pick_up(Item)) :-
	location(Char, Loc),
	location(Item, Loc2),
	id_name(Char, Name),
	id_name(Item, IName),
	(
		Loc = Loc2,
		write_debug_message('%w picked up the %w.', [Name, IName]),
		pick_up(Char, Item)
		;
		write_debug_message('%w tried to pick up the %w, but they couldn\' find it.', [Name, IName])
	),
	complete_goal(Char, pick_up(Item)).
	
do_action(Char, talk(Char2)) :-
	false.

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
	call_story(Type, Item),
	Item \= Char,
	Priority2 is Priority+1,
	create_goal(Char, pick_up(Item), Priority2).

find_item_type(Char, Type, Priority) :-
	location(Char, Location),
	\+ (believes(Char, location(Item, Location)), call_story(Type, Item), Item \= Char),
	\+ known_about_recently(Char, Location),
	Priority2 is Priority+1,
	create_goal(Char, look, Priority2).

% characters can always see other characters in the same place.
find_item_type(Char, _, Priority) :-
	location(Char, Location),
	location(Char2, Location),
	character(Char2),
	\+ talked_recently(Char, Char2),
	Char \= Char2,
	Priority2 is Priority+1,
	create_goal(Char, talk(Char2), Priority2).

find_item_type(Char, Type, Priority) :-
	location(Char, Location),
	believes(Char, location(Item, Somewhere)),
	call_story(Type, Item),
	Char \= Item,
	Somewhere \= Location,
	Priority2 is Priority+1,
	create_goal(Char, travel(Somewhere), Priority2).

find_item_type(Char, Type, Priority) :-
	\+ (believes(Char, location(Item, _)), call_story(Type, Item)),
	Priority2 is Priority+1,
	create_goal(Char, explore, Priority2).

pick_up(Char, Item) :-
	location(Char, Location),
	location(Item, Location),
	Char \= Item,
	retract_story(location(Item, _)),
	broadcast_assert_story(location(Item, Char), Location).

has_item(Char, Type, Item) :-
	location(Item, Char),
	call_story(Type, Item).