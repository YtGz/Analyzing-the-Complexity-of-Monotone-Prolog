append([],Ys,Ys).
append([X|Xs],Ys,[X|Zs]) :- append(Xs,Ys,Zs).

sublist(X,Y) :- append(P,_,Y), append(_,X,P).

:- sublist([1,2],[1,2]).
