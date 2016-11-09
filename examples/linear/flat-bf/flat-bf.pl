
flat([],[]).
flat([[]|T],R) :- flat(T,R).
flat([[H|T]|TT], [H|R]) :- flat([T|TT],R).

:- flat([3,[2,[3]]],X).
