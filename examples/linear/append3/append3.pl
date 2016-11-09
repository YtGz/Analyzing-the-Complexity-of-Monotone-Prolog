app3_a(Xs, Ys, Zs, Us) :- app(Xs, Ys, Vs), app(Vs, Zs, Us).

app([], Ys, Ys).
app([X | Xs], Ys, [X | Zs]) :- app(Xs, Ys, Zs).

:- app3_a([1,2,3],[4,5],[6,7,8],Xs).
