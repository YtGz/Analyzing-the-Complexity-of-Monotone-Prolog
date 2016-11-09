rotate(X,Y) :- append(A,B,X), append(B,A,Y).

append([X|Xs],Ys,[X|Zs]) :- append(Xs,Ys,Zs).
append([],Ys,Ys).


:- rotate([1,2,3],Ys).
