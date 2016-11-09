%% len(Xs, X) :- X is the length of the list Xs.

len([], 0).
len([_ | Ts], s(N)) :- len(Ts, N).


:- len([1,2,5,3],X).
