ackermann(0,N,s(N)).
ackermann(s(M),0,X) :- ackermann(M,s(0),X).
ackermann(s(M),s(N),X) :- ackermann(s(M),N,Y),ackermann(M,Y,X).

:- ackermann(0,0,X).
