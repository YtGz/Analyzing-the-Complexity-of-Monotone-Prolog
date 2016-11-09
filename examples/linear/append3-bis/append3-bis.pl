app3_b(Xs, Ys, Zs, Us) :- app(Ys, Zs, Vs), app(Xs, Vs, Us).

app([], Ys, Ys).
app([X | Xs], Ys, [X | Zs]) :- app(Xs, Ys, Zs).

:- app3_b([1,2,3],[4,5],[6,7,8],Xs).
