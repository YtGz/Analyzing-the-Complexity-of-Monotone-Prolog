div(X,Y,Z) :- quot(X,Y,Y,Z).
quot(0,s(Y),s(Z),0).
quot(s(X),s(Y),Z,U) :- quot(X,Y,Z,U).
quot(X,0,s(Z),s(U)) :- quot(X,s(Z),s(Z),U).

:- div(s(0),s(s(0)),Z).
