select(X,[X|Xs],Xs).
select(X,[Y|Xs],[Y|Zs]) :- select(X,Xs,Zs).

:- select(X,[1,4,3],Zs).
