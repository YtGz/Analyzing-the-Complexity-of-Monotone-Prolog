gopher(nil,nil).
gopher(cons(nil,Y),cons(nil,Y)).
gopher(cons(cons(U,V),W),X) :-  gopher(cons(U,cons(V,W)),X).

:- gopher(cons(cons(nil,nil),nil),Y).
