a(X) :- b(X), q(X).
b(X).
b(X) :- p(X).
p(s(X)) :- p(X).
q(s(X)) :- a(X).

:- a(0).
