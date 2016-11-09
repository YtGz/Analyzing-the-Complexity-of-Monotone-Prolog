%% list(Xs) :-  Xs is a list.

list([]).
list([_ | Ts]) :- list(Ts).

:- list([5,2,4]).
