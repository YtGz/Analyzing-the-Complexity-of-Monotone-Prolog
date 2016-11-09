p(s(0),0).
p(s(s(X)),s(s(Y))) :- p(s(X),s(Y)).
plus(0,Y,Y).
plus(s(X),Y,s(Z)) :- p(s(X),U), plus(U,Y,Z).

:- plus(s(s(0)),Y,Z).
