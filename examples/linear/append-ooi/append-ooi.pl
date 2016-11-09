app([],X,X).
app([X|Xs],Ys,[X|Zs]) :-
	app(Xs,Ys,Zs).

:- app(X,Y,[1,2,3]).
