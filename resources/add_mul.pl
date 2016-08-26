add(0,X,X).
add(s(Y),X,s(Z)) :- add(Y,X,Z).

:- add(0,0,X).

/*mult(0,X) :- 0.
mult(s(Z),X) :- add(X,mult(Z,X)).

% :- add(X,s(0)).
:- mult(s(0),X).*/

/* ackermann(0,N,X) :- X is N+1.
ackermann(M, 0, X) :- M>0, M1 is M-1, ackermann(M1, 1, X).
ackermann(M, N, X) :- M>0, N>0, M1 is M-1, N1 is N-1, ackermann(M, N1, X1), ackermann(M1, X1, X).*/

/*pred(0) :- 0.
pred(s(X)) :- X.*/
/*
ackermann(0,N,s(N)).
ackermann(s(M),0,X) :- ackermann(M,s(0),X).
ackermann(s(M),s(N),X) :- ackermann(s(M),N,Y),ackermann(M,Y,X).

:- ackermann(0,0,X).*/
