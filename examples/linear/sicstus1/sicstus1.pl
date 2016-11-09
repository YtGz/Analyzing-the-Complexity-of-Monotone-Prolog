reverse_concatenate([], L, L).
reverse_concatenate([X|L1], L2, L3) :-
        reverse_concatenate(L1, [X|L2], L3).

:- reverse_concatenate([1,2,3],[4,5],Zs).
