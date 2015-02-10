link_start("https://")-->"https://".
link_start("http://")-->"http://".
flowdown_link(X)-->
	link_start(Start), something(Middle), [Stop],
	{
		char_type(Stop, white),
		merge(Start, Middle, X)
	}.


flowdown_token(flowdown_link(X))-->flowdown_link(X).

flowdown_paragraph([]) -->
	[A,B], 
	{
		char_type(A, end_of_line),
		char_type(B, end_of_line)
	}.
flowdown_paragraph([H|T]) --> 
	flowdown_token(H), flowdown_paragraph(T).
flowdown_paragraph([H|T]) -->
	[H], flowdown_paragraph(T).


flowdown([flowdown_paragraph(W)| T]) -->
	flowdown_paragraph(W),gobble_ends, flowdown(T).

postprosses([flowdown_link(X)|Tin], [flowdown_link(Y)| Tout], []):- 
	string_to_list(Y, X),
	postprosses(Tin, Tout, []).
postprosses([flowdown_link(X)|Tin], [Z, flowdown_link(Y)| Tout], Accimulated):-
	string_to_list(Z, Accimulated),
	string_to_list(Y, X),
	postprosses(Tin, Tout).

flowdown([flowdown_paragraph(H)])-->something_gready(H).
flowdown([])-->[].

gobble_ends-->[H], {char_type(H, end_of_line)}, gobble_ends.
gobble_ends-->[].

something([])-->[].
something([H|T])-->[H], something(T).
something_gready([H|T])-->[H], something_gready(T).
something_gready([H])-->[H].

flowdown_to_html([], []).
flowdown_to_html(flowdown(X), html(Y)):-flowdown_to_html(X, Y).
flowdown_to_html([flowdown_paragraph(X)|Tf], [p(Y)| Th]):- string_to_list(Y, X),flowdown_to_html(Tf, Th).