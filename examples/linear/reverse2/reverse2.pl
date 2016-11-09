reverse(L, LR) :- revacc(L, LR, []).

revacc([], L, L).
revacc([EL|T], R, A) :- revacc(T, R, [EL|A]).

:- reverse([1,2,3],Ys).
