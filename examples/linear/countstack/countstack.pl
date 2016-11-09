countstack(empty,0).
countstack(push(nil,T),X) :- countstack(T,X).
countstack(push(cons(U,V),T),s(X)) :- countstack(push(U,push(V,T)),X).

:- countstack(empty,Y).
