rev(LS,RES) :- r1(LS,[],RES).
r1([],RES,RES).
r1([X|Xs],Accm,RES) :-
	r1(Xs,[X|Accm],RES).

:- rev([1,2,3],Ys).
