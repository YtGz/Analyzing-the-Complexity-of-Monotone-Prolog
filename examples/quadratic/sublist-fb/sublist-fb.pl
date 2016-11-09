%% sublist(Xs, Ys) :- Xs is a sublist of the list Ys.
%%

sublist(Xs, Ys) :- app(_, Zs, Ys), app(Xs, _, Zs).

app([],X,X).
app([X|Xs],Ys,[X|Zs]) :-
	app(Xs,Ys,Zs).


:- sublist(Xs,[1,2,6,9]).
