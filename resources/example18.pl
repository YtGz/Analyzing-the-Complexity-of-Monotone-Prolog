app([],YS,YS).
app([X|XS],YS,[X|ZS]) :- app(XS,YS,ZS).
sublist(X,Y) :- app(P,U,Y), app(V,X,P).

:- sublist(X,0).
