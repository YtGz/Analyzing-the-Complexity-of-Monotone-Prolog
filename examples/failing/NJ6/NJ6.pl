f(A,[],RES) :- g(A,[],RES).
f([A|As],[B|Bs],RES) :- f([B|[A|As]],Bs,RES).

g([],RES,RES).
g([C|Cs],D,RES) :- g(Cs,[C|D],RES).

:- f([1,2,3],[4,5],Z).
