p(X,Y).
map([X|Xs],[Y|Ys]) :- p(X,Y),map(Xs,Ys).
map([],[]).

:- map([3,4],Y).
