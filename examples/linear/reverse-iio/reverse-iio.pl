reverse([],X,X).
reverse([X|Y],Z,U) :- reverse(Y,Z,[X|U]).

:- reverse([1,2,3],[3,2,1],Zs).