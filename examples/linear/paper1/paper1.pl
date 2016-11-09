p(X,X).
p(f(X),g(Y)) :- p(f(X),f(Z)), p(Z,g(Y)).

:- p(f(f(h)),Y).
