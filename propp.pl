:- module(propp, [generate_initial_move/1]).

proppian_function(alpha, initial, req([], [])).
proppian_function(beta, absentation, req([], [])).
proppian_function(gamma, interdiction, req([], delta)).
proppian_function(delta, violation, req(gamma, [])).
proppian_function(epsilon, reconnaissance, req([], zeta)).
proppian_function(zeta, delivery, req([], [])).
proppian_function(eta, trickery, req([], theta)).
proppian_function(theta, complicity, req([], [])).
proppian_function(a, villainy, req([], k)).
proppian_function(a2, lack, req([], k)).
proppian_function(b, mediation, req([], [])).
proppian_function(c, beginning_counteraction, req([], [])).
proppian_function(up_arrow, departure, req([], [])).
proppian_function(d, first_proppian_function_donor, req([], e)).
proppian_function(e, reaction, req([], [])).
proppian_function(f, receipt_of_magical_agent, req([], [])).
proppian_function(g, guidance, req([], [])).
proppian_function(h, struggle, req([], j)).	
proppian_function(i, branding, req([], [])).
proppian_function(j, victory, req(h, [])).
proppian_function(k, liquidation, req([a, a2], [])).
proppian_function(down_arrow, return, req(up_arrow, [])).
proppian_function(pr, pursuit, req([], rs)).
proppian_function(rs, rescue, req(pr, [])).
proppian_function(o, unrecognised_arrival, req([], q)).
proppian_function(l, unfounded_claims, req([], ex)).
proppian_function(m, difficult_task, req([], n)).
proppian_function(n, solution, req(m, [])).
proppian_function(q, recognition, req(o, [])).
proppian_function(ex, exposure, req(l, [])).
proppian_function(t, transfiguration, req([], [])).
proppian_function(u, punishment, req([], [])).
proppian_function(w, wedding, req([], [])).

satisfies_prereqs(Moves, F) :-
	proppian_function(F, _, Req),
	(
		Req = req([], _) ; 
		Req = req(Pre, _), member(Pre, Moves)
		;
		format('~s does not satisfy some prereqs\n', F), !, fail
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
	findall(X, proppian_function(X, _, _), Functions),
	pick_functions(Functions, Moves, []),
	include(satisfies_prereqs(Moves), Moves, Move).

pick_functions([], [], _).
%if we need F, then pick it (and its requirements too).
pick_functions([F|Fs], [F|Fs2], Needed) :-
	Needed \= [],
	member(F, Needed),
	delete(F, Needed, StillNeeded),
	proppian_function(F, _, req(_, N)),
	pick_functions(Fs, Fs2, [N|StillNeeded]).

% otherwise flip a coin
pick_functions([F|Fs], [F|Fs2], Needed) :-
	maybe(0.7),
	proppian_function(F, _, req(_, N)),
	pick_functions(Fs, Fs2, [N|Needed]).

pick_functions([_|Fs], Fs2, Needed) :-
	pick_functions(Fs, Fs2, Needed).



