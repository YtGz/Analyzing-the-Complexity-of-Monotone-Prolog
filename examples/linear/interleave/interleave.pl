interleave([],Xs,Xs).
interleave([X|Xs],Ys,[X|Zs]) :-
	interleave(Ys,Xs,Zs).

:- interleave([1,2],[3,4],Zs).
