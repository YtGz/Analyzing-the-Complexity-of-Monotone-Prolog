% transitive closure of a relation p (Krishna Rao et al.)

p(a, b).
p(b, c).

tc(X, X).
tc(X, Y) :- p(X, Z), tc(Z, Y).


:- tc(a,Y).
