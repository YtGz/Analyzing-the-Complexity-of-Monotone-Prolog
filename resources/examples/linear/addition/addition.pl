add(0,X,X).
add(s(Y),X,s(Z)) :- add(Y,X,Z).

:- add(0,0,X).
