% Checks if two binary trees have the same fringe.

sameleaves(leaf(L),leaf(L)).
sameleaves(tree(TR1,TR2),tree(S1,S2)) :-
		getleave(TR1,TR2,L,TR),
		getleave(S1,S2,L,S),
		sameleaves(TR,S).

getleave(leaf(A),C,A,C).
getleave(tree(A,B),C,L,O) :- getleave(A,tree(B,C),L,O).

:- sameleaves(leaf(a),leaf(a)).
