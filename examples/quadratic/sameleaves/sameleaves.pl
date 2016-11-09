% by: Bart Demoen <bmd@cs.kuleuven.ac.be>

sameleaves(leaf(L),leaf(L)).
sameleaves(tree(TR1,TR2),tree(S1,S2)) :-
                getleaf(TR1,TR2,L,TR),
                getleaf(S1,S2,L,S),
                sameleaves(TR,S).

getleaf(leaf(A),C,A,C).
getleaf(tree(A,B),C,L,O) :- getleaf(A,tree(B,C),L,O).

:- sameleaves(tree(tree(leaf('a'),leaf('b')),leaf('c')),tree(leaf('a'),tree(leaf('b'),leaf('c')))).
