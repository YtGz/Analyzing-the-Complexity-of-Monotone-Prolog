append([],Ys,Ys).
append([X|Xs],Ys,[X|Zs]) :- append(Xs,Ys,Zs).

sublist(X,Y) :- append(P,_,Y), append(_,X,P).

:- sublist(Xs,[1,2,3]).
