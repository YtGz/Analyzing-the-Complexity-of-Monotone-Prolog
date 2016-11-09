%% ordered(Xs) :- Xs is an lte-ordered list of integers.

ordered([]).
ordered([_]).
ordered([X, Y | Xs]) :- less(X,s(Y)), ordered([Y| Xs]).

less(0, s(_)).
less(s(X), s(Y)) :- less(X, Y).


:- ordered([0,s(s(0))]).
