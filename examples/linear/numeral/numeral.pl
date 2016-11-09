%% num(X) :- X is a numeral.

num(0).
num(s(X)) :- num(X).

:- num(s(s(s(0)))).
