%% member(Element, List) :- Element is an element of the list List.

member(X, [X | _]).
member(X, [_ | Xs]) :- member(X, Xs).

:- member(X,[1,2,5,4]).
