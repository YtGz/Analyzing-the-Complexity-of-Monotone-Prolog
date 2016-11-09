even(0).
even(s(s(0))).
even(s(s(s(X)))) :- odd(X).
odd(s(0)).
odd(s(X)) :- even(s(s(X))).

:- even(s(s(s(0)))).
