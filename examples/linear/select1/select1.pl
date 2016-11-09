select(X,[X|Xs],Xs).
select(X,[Y|Xs],[Y|Zs]) :- select(X,Xs,Zs).

:- select(3,[1,5,3,2],Zs).
