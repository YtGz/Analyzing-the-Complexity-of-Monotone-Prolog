%% suffix(Xs, Ys) :- Xs  is a suffix of the list Ys.

suffix(Xs, Ys) :- app(_, Xs, Ys).

app([],X,X).
app([X|Xs],Ys,[X|Zs]) :-
        app(Xs,Ys,Zs).


:- suffix(Xs,[1,2,5,4]).
