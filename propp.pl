:- module(propp, [generate_initial_move/1]).
:- use_module(world).
:- use_module(story).
:- use_module(utils).

% outlines and requirements of each proppian function.
proppian_function(alpha, initial, req([], []), 'an introduction to the hero.').
proppian_function(beta, absentation, req([], []), 'one of the members of a family absents themself from home.').
proppian_function(gamma, interdiction, req([], delta), 'an interdiction is given to the hero.').
proppian_function(delta, violation, req(gamma, []), 'the interdiction is violated.').
proppian_function(epsilon, reconnaissance, req([], zeta), 'the villain makes an attempt at reconnaissance.').
proppian_function(zeta, delivery, req([], []), 'the villain receives information about their victim.').
proppian_function(eta, trickery, req([], theta), 'the villain attempts to deceive their victim to take possession of them/their belongings.').
proppian_function(theta, complicity, req([], []), 'the victim submits to deception and thereby unwittingly helps their enemy.').
proppian_function(a, villainy, req([], k), 'the villain causes harm or injury to a member of the family.').
proppian_function(a2, lack, req([], k), 'one member of a family either lacks something or desires something.').
proppian_function(b, mediation, req([], []), 'misfortune or lack is made known; the hero is approached with a request or command; they are allowed to go or are dispatched.').
proppian_function(c, beginning_counteraction, req([], []), 'the seeker agrees to or decides upon counteraction.').
proppian_function(up_arrow, departure, req([], []), 'the hero leaves home.').
proppian_function(d, first_proppian_function_donor, req([], e), 'the hero is tested.').
proppian_function(e, reaction, req([], []), 'the hero reacts to the actions of the future donor.').
proppian_function(f, receipt_of_magical_agent, req([], []), 'the hero acquires the use of a magical agent.').
proppian_function(g, guidance, req([], []), 'the hero is transferred to the whereabouts of an object of search.').
proppian_function(h, struggle, req([], j), 'the hero and villain join in direct combat.').	
proppian_function(i, branding, req([], []), 'the hero is branded.').
proppian_function(j, victory, req(h, []), 'the villain is defeated.').
proppian_function(k, liquidation, req([a, a2], []), 'the initial misfortune is liquidated').
proppian_function(down_arrow, return, req(up_arrow, []), 'the hero returns.').
proppian_function(pr, pursuit, req([], rs), 'the hero is pursued').
proppian_function(rs, rescue, req(pr, []), 'the hero is rescued from pursuit.').
proppian_function(o, unrecognised_arrival, req([], q), 'the hero, unrecognised, arrives home or in another country.').
proppian_function(l, unfounded_claims, req([], ex), 'a false hero presents unfounded claims.').
proppian_function(m, difficult_task, req([], n), 'a difficult task is proposed to the hero.').
proppian_function(n, solution, req(m, []), 'the task is resolved.').
proppian_function(q, recognition, req(o, []), 'the hero is recognised.').
proppian_function(ex, exposure, req(l, []), 'the false hero is exposed.').
proppian_function(t, transfiguration, req([], []), 'the hero is given a new appearance.').
proppian_function(u, punishment, req([], []), 'the villain is punished.').
proppian_function(w, wedding, req([], []), 'the hero receives a reward.').

% what needs to be done to setup each function.
propp_setup(alpha) :-
	init_story_creation,
	create_world(Start),
	create_hero(Hero, Start), !.

propp_setup(beta) :-
	hero(Hero),
	region(R),
	random_member(Func, [add_story_precondition(not(location(Hero, R)))]),
	call(world:Func).
	
propp_setup(beta, []).

satisfies_prereqs(Moves, F) :-
	proppian_function(F, _, Req, _),
	(
		Req = req([], _) ; 
		Req = req(Pre, _), member(Pre, Moves) ;
		Req = req([Pre1, Pre2], _), (member(Pre1, Moves), member(Pre2, Moves)) ;
		%format('~s does not satisfy some prereqs\n', F), 
		!, fail
	).

generate_initial_move([alpha|Moves]) :-
	generate_move(Move),
	(
		Move = [alpha|T], !,
		Moves = T
		;
		Moves = Move
	).

generate_move(Move) :-
	findall(X, proppian_function(X, _, _, _), Functions),
	(maybe(0.7), Villainy = a ; Villainy = a2),
	pick_functions(Functions, Moves, [Villainy]),
	include(satisfies_prereqs(Moves), Moves, Move).

pick_functions([], [], _).
%if we need F, then pick it (and its requirements too).
pick_functions([F|Fs], [F|Fs2], Needed) :-
	Needed \= [],
	member(F, Needed),
	delete(F, Needed, StillNeeded),
	proppian_function(F, _, req(_, N), _),
	pick_functions(Fs, Fs2, [N|StillNeeded]).

% otherwise weight it; the chance is fairly high because otherwise it'll miss the first
% of the pair, and then have to remove the second of the pair..also longer ones are better
pick_functions([F|Fs], [F|Fs2], Needed) :-
	maybe(0.75),
	proppian_function(F, _, req(_, N), _),
	pick_functions(Fs, Fs2, [N|Needed]).

pick_functions([_|Fs], Fs2, Needed) :-
	pick_functions(Fs, Fs2, Needed).

generate_plot_overview(Fs) :-
	generate_plot_overview(Fs, first).

generate_plot_overview([], _) :-
	format('The end.').

generate_plot_overview([F|Fs], First) :-
	proppian_function(F, _, _, Desc),
	(First = first, write('Once upon a time, ') ; First = last, write('And finally, ') ; maybe(0.33), write('Then, ') ; maybe(0.5), write('Soon after, ') ; write('Later, ')),
	write(Desc), nl,
	(Fs = [], Next = last ; Next = none),
	generate_plot_overview(Fs, Next).