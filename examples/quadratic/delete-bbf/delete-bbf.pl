%% delete(Element, Tree, Tree1) :- Tree1 is the result of deleting
%%                                 Element from the search tree Tree.

delete(X, tree(X, void, Right), Right).
delete(X, tree(X, Left, void), Left).
delete(X, tree(X, Left, Right), tree(Y, Left, Right1)) :-
 	delmin(Right, Y, Right1).
delete(X, tree(Y, Left, Right), tree(Y, Left1, Right)) :-
	less(X,Y), delete(X, Left, Left1).
delete(X, tree(Y, Left, Right), tree(Y, Left, Right1)) :-
 	less(Y,X), delete(X, Right, Right1).

delmin(tree(Y, void, Right), Y, Right).
delmin(tree(X, Left, _), Y, tree(X, Left1, _)) :-
 	delmin(Left, Y, Left1).

less(0, s(_)).
less(s(X), s(Y)) :- less(X, Y).


:- delete(3,tree(3,void,void),X).
