%% palindrome(Xs) :- the list Xs equals to its reverse.

palindrome(Xs) :- reverse(Xs, Xs).

reverse(X1s, X2s) :- reverse(X1s, [], X2s).

reverse([], Xs, Xs).
reverse([X | X1s], X2s, Ys) :- reverse(X1s, [X | X2s], Ys).


:- palindrome([3,4,6]).
