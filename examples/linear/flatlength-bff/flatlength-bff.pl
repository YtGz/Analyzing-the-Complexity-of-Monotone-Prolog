fl([],[],0).
fl([E|X],R,s(Z)) :-
        append(E,Y,R),
        fl(X,Y,Z).

append([],X,X).
append([X|Xs],Ys,[X|Zs]) :-
        append(Xs,Ys,Zs).

:- fl([],Xs,Ys).
