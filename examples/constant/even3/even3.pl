% The even predicate using unification without occur check and lists
% Author: Thomas Stroeder

e(X) :- Y = [e,o|Y], c(Y,X).
c([e|_],0).
c([_|X],s(Y)) :- c(X,Y).

:- e(s(s(0))).