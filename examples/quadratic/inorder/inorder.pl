inorder(nil,[]).
inorder(tree(L,V,R),I) :-
	inorder(L,LI),
	inorder(R,RI),
	append(LI,[V|RI],I).

append([],X,X).
append([X|Xs],Ys,[X|Zs]) :-
	append(Xs,Ys,Zs).

:- inorder(tree(nil,2,tree(nil,1,nil)),Y).
