append([], X, X).
append([X|Y], U, [X|Z]) :- append(Y, U, Z).

:- append(Xs,Ys,[1,2,3,4]).
