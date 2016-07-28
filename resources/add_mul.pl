add(0,X) :- X.
add(s(Z),X) :- s(add(Z,X)).

mult(0,X) :- 0.
mult(s(Z),X) :- add(X,mult(Z,X)).

:- add(X,s(0)).
