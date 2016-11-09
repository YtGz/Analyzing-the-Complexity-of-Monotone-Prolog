%% sum(X, Y, Z) :- X, Y, Z are numerals such that Z is the sum of X and Y.

sum(X, 0, X).
sum(X, s(Y), s(Z)) :- sum(X, Y, Z).


:- sum(X,s(s(0)),Z).
