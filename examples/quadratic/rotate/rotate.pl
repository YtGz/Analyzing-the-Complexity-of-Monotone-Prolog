rotate(X,Y) :- append2(A,B,X), append1(B,A,Y).

append1([X|Xs],Ys,[X|Zs]) :- append1(Xs,Ys,Zs).
append1([],Ys,Ys).

append2([X|Xs],Ys,[X|Zs]) :- append2(Xs,Ys,Zs).
append2([],Ys,Ys).

:- rotate([1,2,3],Ys).
