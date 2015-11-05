:- module(utils, [write_line/1, write_line/2, writef_line/2, write_debug_message/2,
	fact/1, fact_story/1,
	generate_id/2,
	random_line_from_file/3, random_line_from_file/4]).

debug_log.

fact(Fact):-
	clause(Fact, true).
fact_story(Fact) :-
	clause(:(story, Fact), true).

cleanup_ref(Refs):-
	forall(member(X, Refs), erase(X)).

%% Easier writing predicates
write_debug_message(Message, Stuff):-
	\+ debug_log, !
	;
	writef_line(Message, Stuff).

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

alphanumeric(['Q','W','E','R','T','Y','U','I','O','P','A','S','D','F','G','H','J''K','L','Z','X','C','V','B','N','M',
	q,w,e,r,t,y,u,i,o,p,a,s,d,f,g,h,j,k,l,z,x,c,v,b,n,m,1,2,3,4,5,6,7,8,9,0]).

make_truncated_uuid(IDStr):-
	make_uuid(UUID, 8, []),
	atom_string(UUID, IDStr).

generate_id(Name, ID):-
	(
		atom_length(Name, Len),
		Len < 8,
		NameLen = Len
		;
		NameLen = 8
	),
	sub_string(Name, _, NameLen, _, SubStr),
	make_truncated_uuid(UUID),
	string_concat(SubStr, UUID, IDStr),
	atom_string(ID, IDStr), !.
	%NOTE MIGHT DOUBLE UP ON IDS

make_uuid(ID, 0, ID_List) :-
	atomic_list_concat(ID_List, '', ID).

make_uuid(ID, Len, Xs) :-
	alphanumeric(Alph),
	random_member(X, Alph),
	Len2 is Len-1,
	make_uuid(ID, Len2, [X|Xs]).