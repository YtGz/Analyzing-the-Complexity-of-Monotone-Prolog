%% reverse(Xs, Ys) :- Ys is the reverse of the list Xs.

reverse(X1s, X2s) :- reverse(X1s, [], X2s).

reverse([], Xs, Xs).
reverse([X | X1s], X2s, Ys) :- reverse(X1s, [X | X2s], Ys).


:- reverse([5,6,3],Ys).
