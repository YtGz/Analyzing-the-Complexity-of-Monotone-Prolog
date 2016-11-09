
transpose(A,B) :-
        transpose_aux(A,[],B).

transpose_aux([R|Rs],_,[C|Cs]) :-
    row2col(R,[C|Cs],Cols1,[],Accm),
    transpose_aux(Rs,Accm,Cols1).
transpose_aux([], X, X).

row2col([X|Xs],[[X|Ys]|Cols],[Ys|Cols1],A,B) :-
    row2col(Xs,Cols,Cols1,[[]|A],B).
row2col([],[],[],A,A).


:- transpose([[1,2]],Y).
