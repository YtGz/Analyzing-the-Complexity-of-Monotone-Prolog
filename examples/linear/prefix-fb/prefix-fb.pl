%% prefix(Xs, Ys) :- Xs is a prefix of the list Ys.

prefix(Xs, Ys) :- app(Xs, _, Ys).

app([],X,X).
app([X|Xs],Ys,[X|Zs]) :-
	app(Xs,Ys,Zs).


:- prefix(Xs,[2,5,1,4]).
