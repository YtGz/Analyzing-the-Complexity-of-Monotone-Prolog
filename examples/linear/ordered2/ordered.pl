ordered([]).
ordered([X]).
ordered([X,Y|Xs]) :- le(X,Y), ordered([Y|Xs]).

le(s(X), s(Y)) :- le(X, Y).
le(0, s(0)).
le(0, 0).

:- ordered([0,s(0),s(s(s(0)))]).
