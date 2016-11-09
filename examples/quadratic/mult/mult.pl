%% mult(X, Y, Z) :- X, Y, Z are numerals such that Z is the product of X and Y.
%%

mult(_, 0, 0).
mult(X, s(Y), Z) :- mult(X, Y, W), sum(W, X, Z).

sum(X, 0, X).
sum(X, s(Y), s(Z)) :- sum(X, Y, Z).

:- mult(s(s(0)),s(0),Z).
