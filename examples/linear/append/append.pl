app([X|Xs],Ys,[X|Zs]) :- app(Xs,Ys,Zs).
app([],Ys,Ys).


:- app(Xs,[0],[s(0),s(s(0)),0]).
