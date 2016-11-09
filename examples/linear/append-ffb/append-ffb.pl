%% app(Xs, Ys, Zs) :- Zs is the result of concatenating the lists Xs and Ys.
%%

app([],X,X).
app([X|Xs],Ys,[X|Zs]) :-
	app(Xs,Ys,Zs).

:-app(X,Y,[]).
