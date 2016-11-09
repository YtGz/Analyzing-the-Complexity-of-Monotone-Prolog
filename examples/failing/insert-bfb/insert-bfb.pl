%% insert(Element, Tree, Tree1) :- Tree1 is the result of inserting
%%                                 Element in the search tree Tree.

insert(X, void, tree(X, void, void)).
insert(X, tree(X, Left, Right), tree(X, Left, Right)).
insert(X, tree(Y, Left, Right), tree(Y, Left1, Right)) :-
	less(X,Y), insert(X, Left, Left1).
insert(X, tree(Y, Left, Right), tree(Y, Left, Right1)) :-
 	less(Y,X), insert(X, Right, Right1).

less(0, s(_)).
less(s(X), s(Y)) :- less(X, Y).


:- insert(0,X,tree(0,void,void)).
