rem(X, Y, R) :- notZero(Y), sub(X, Y, Z), rem(Z, Y, R).
rem(X, Y, X) :- notZero(Y), geq(X, Y).

sub(s(X), s(Y), Z) :- sub(X, Y, Z).
sub(X, 0, X).

notZero(s(X)).

geq(s(X), s(Y)) :- geq(X, Y).
geq(X, 0).


:- rem(s(s(0)),s(0),Z).
