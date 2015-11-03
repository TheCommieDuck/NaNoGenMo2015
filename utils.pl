:- module(utils, [write_line/1, write_line/2, writef_line/2, write_debug_message/2,
	fact/1,
	generate_id/2,
	random_line_from_file/3, random_line_from_file/4]).

debug_log.

fact(Fact):-
	clause(Fact, true).

cleanup_ref(Refs):-
	forall(member(X, Refs), erase(X)).

%% Easier writing predicates
write_debug_message(Message, Stuff):-
	\+ debug_log
	;
	format(Message, Stuff), nl.

write_line(Stuff):-
	write(Stuff), nl.

write_line(Stream, Stuff):-
	write(Stream, Stuff),
	nl(Stream),
	flush_output(Stream).

writef_line(Stuff, List):-
	writef(Stuff, List), nl.

% Random line selection from a file
random_line_from_file(File, Lines, Delim, Atoms) :-
	open(File, read, Stream),
	Line is random(Lines),
	read_line_to_atom(Stream, Line, Atom),
	atomic_list_concat(Atoms, Delim, Atom),
	close(Stream).

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

% ID generation
make_truncated_uuid(IDStr):-
	uuid(UUID),
	atom_string(UUID, UUIDString),
	sub_string(UUIDString, _, 8, 0, IDStr).

generate_id(Name, ID):-
	make_truncated_uuid(UUID),
	sub_string(Name, _, 8, _, SubStr),
	string_concat(SubStr, UUID, ID),
	\+ (story:location(ID) ; story:character(ID)).