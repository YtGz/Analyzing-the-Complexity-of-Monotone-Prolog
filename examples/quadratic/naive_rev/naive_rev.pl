app([],Ys,Ys).
app([X|Xs],Ys,[X|Zs]) :- app(Xs,Ys,Zs).

reverse([],[]).
reverse([X|Xs],Ys) :- reverse(Xs,Zs), app(Zs,[X],Ys).

:- reverse([1,2,3],Ys).
