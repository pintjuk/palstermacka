:-use_module(library(http/http_client)).

nt1, [b]-->[a].
nt2		-->[b].

suffix([]) --> []. 
anything --> [].
anything --> [_], anything.

uid([H|T]) -->
	[H],
	{
		\+code_type(H, quote)
	},
	uid(T).
uid([]) --> [].



compaind_zeros, [zero(X+1)]-->
	[zero(X), 0], compaind_zeros.
compaind_zeros, [zero(1)]-->
	[0], compaind_zeros.
compaind_zeros -->
	[].
zero(X)-->
	compaind_zeros,[zero(X)]. 
saprse([zero(X)|T])-->
	zero(X), saprse(T).
saprse([H|T])-->
	[H], saprse(T).

saprse([])-->[].


%% zero(X)-->
%% 	{
%% 		X>1, Z is X-1
%% 	},
%% 	[0],
%% 	zero(Z).
	
%% zero(1)-->[0].

lel(Uid)-->"{\"user\":\"", uid(Uid),"\"}".
wat(Uid)-->anything, uid(Uid), anything.
get_ruid(Result):-
	http_get('http://login.datasektionen.se/verify/lFxUh93Ar2j8VCLBK7K0VA.json', Result, []).

