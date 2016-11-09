append([H|X],Y,[X|Z]) :- append(X,Y,Z).
append([],Y,Y).

:- append([1,2],[3,4],Zs).
