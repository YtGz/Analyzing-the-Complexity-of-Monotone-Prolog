%% rev(Xs, Ys) :- Ys is the result of reversing the list Xs.
%%

rev([], []).
rev([X | Xs], Ys) :- rev(Xs, Zs), app(Zs, [X], Ys).

app([],X,X).
app([X|Xs],Ys,[X|Zs]) :-
	app(Xs,Ys,Zs).


:- rev([1,4,3],Ys).
