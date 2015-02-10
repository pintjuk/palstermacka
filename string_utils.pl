:- module(string_utils,
			[
				op(100, yfx, squashTo),
				squashTo/2
			]).

squashTo([A], A).
squashTo([A, B| T], H):-
	string_concat(A, B, C),
	squashTo([C|T], H).