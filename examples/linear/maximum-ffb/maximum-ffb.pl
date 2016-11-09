%% max(X, Y, Z) :- Z is the maximum of the integers X and Y.

max(X, Y, X) :- less(Y,X).
max(X, Y, Y) :- less(X,s(Y)).

less(0, s(_)).
less(s(X), s(Y)) :- less(X, Y).


:- max(X,Y,s(s(s(0)))).
