%% len1(Xs, X) :- X is the length of the list Xs.

len1([], 0).
len1([_ | Ts], N) :- len1(Ts, M), eq(N,s(M)).

eq(X,X).

:- len1([3,4,1,6],X).
