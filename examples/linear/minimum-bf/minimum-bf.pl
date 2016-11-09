%% minimum(Tree, Element) :- Element is the minimum element of
%%                          the search tree Tree.

minimum(tree(X, void, _), X).
minimum(tree(_, Left, _), X) :- minimum(Left, X).

:- minimum(tree(3,void, void),X).
