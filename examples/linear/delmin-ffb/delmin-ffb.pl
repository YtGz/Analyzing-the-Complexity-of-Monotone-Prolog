delmin(tree(Y, void, Right), Y, Right).
delmin(tree(X, Left, _), Y, tree(X, Left1, _)) :-
 	delmin(Left, Y, Left1).

:- delmin(X,Y,tree(3,void,void)).
