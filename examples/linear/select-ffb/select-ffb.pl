%% select(X, Xs, Zs) :- Zs is the result of deleting one occurrence of X
%%                      from the list Xs.

select(X, [X | Xs], Xs).
select(X, [Y | Xs], [Y | Zs]) :- select(X, Xs, Zs).


:- select(X,Xs,[3,1,6,4]).
