member(X,[Y|Xs]) :- member(X,Xs).
member(X,[X|Xs]).

:- member(X,[1,4,2]).
