q(X) :- p(X,0).
p(0,X).
p(s(X),Y) :- p(X,s(Y)).

:- q(0).