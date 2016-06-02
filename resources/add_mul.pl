add(0,X) :- X.
add(s(Z),X) :- s(add(Z,X)).

mult(0,X) :- 0.
mult(s(Z),X) :- add(Z,mult(Z,X)).

:- add(0,s(0)).