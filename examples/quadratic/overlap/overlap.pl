overlap(Xs,Ys) :- member2(X,Xs),member1(X,Ys).

member1(X,[X|Xs]).
member1(X,[Y|Xs]) :- member1(X,Xs).

member2(X,[X|Xs]).
member2(X,[Y|Xs]) :- member2(X,Xs).

:- overlap([a,d,f],[a,b]).
