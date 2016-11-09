
f(c(s(X),Y)) :- f(c(X,s(Y))).
g(c(X,s(Y))) :- g(c(s(X),Y)).

h(X) :- f(X),g(X).

:- h(c(s(0),s(0))).
