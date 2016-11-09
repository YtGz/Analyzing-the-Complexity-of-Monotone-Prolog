f([],RES,RES).
f([Head|Tail],X,RES) :- g(Tail,X,[Head|Tail],RES).

g(A,B,C,RES) :-
	f(A,[B|C],RES).

:- f([1,2],2,Z).
