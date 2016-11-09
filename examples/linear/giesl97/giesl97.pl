f(0,Y,0).
f(s(X),Y,Z) :- f(X,Y,U), f(U,Y,Z).

:- f(s(s(0)),Y,Z).
