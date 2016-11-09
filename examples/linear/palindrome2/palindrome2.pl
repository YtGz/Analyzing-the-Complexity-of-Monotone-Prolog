palindrome(Xs) :- reverse(Xs, Xs).

reverse(X1s, X2s) :- reverse3(X1s, [], X2s).

reverse3([X|X1s], X2s, Ys) :- reverse3(X1s, [X|X2s], Ys).
reverse3([], Xs, Xs).


:- palindrome([1,2,1]).
