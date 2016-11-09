plus(0,Y,Y).
plus(s(X),Y,Z) :- plus(X,s(Y),Z).

:- plus(s(s(0)), Y, Z).
