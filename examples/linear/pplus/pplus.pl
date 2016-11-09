/* plus variant using predecessor */

p(s(X),X).
plus(0,Y,Y).
plus(s(X),Y,s(Z)) :- p(s(X),U), plus(U,Y,Z).

:- plus(s(s(0)),Y,Z).
