:- module(parse_utils,
			[
				anything/2,
				anything_gredy/2
			]).

anything --> [].
anything --> [_], anything.

anything_gredy --> [_], anything_gredy.
anything_gredy --> [].