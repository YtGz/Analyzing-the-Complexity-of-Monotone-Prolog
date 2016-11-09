%% less(X, Y) :- X, Y are numerals such that X lt Y.

less(0, s(_)).
less(s(X), s(Y)) :- less(X, Y).


:- less(X,s(s(s(s(s(0)))))).
