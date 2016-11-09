%% bin_tree(T) :- T is a tree.

bin_tree(void).
bin_tree(tree(_, Left, Right)) :-
	bin_tree(Left),
	bin_tree(Right).

:- bin_tree(tree(3,tree(5,void,void),void)).
