append([],L,L).
append([H|L1],L2,[H|L3]) :- append(L1,L2,L3).

append3(A,B,C,D) :- append(A,B,E), append(E,C,D).

:- append3([1,2],[3,4,5],[6,7],Zs).
