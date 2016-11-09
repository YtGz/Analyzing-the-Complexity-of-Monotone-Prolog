append1([],Ys,Ys).
append1([X|Xs],Ys,[X|Zs]) :- append1(Xs,Ys,Zs).

append2([],Ys,Ys).
append2([X|Xs],Ys,[X|Zs]) :- append2(Xs,Ys,Zs).

sublist(X,Y) :- append1(P,_,Y), append2(_,X,P).


:- sublist([2,3],[1,2,3]).
