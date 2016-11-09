duplicate([],[]).
duplicate([X|Y],[X|[X|Z]]) :- duplicate(Y,Z).

:- duplicate([1,2,3,4,5],Ys).
