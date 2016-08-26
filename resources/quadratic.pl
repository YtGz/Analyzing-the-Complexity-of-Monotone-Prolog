%list reversal - O(n)
rev(Xs,Ys) :- rev_(Xs,[],Ys).
rev_([],Ys,Ys).
rev_([X|Xs],Ys,Zs) :- rev_(Xs,[X|Ys],Zs).

%append - O(n)
app([],Ys,Ys).
app([X|Xs],Ys,[X|Zs]) :- app(Xs,Ys,Zs).

%append with superfluous list reversals - O(n²)
app2([],Ys,Ys).
app2([X|Xs],Ys,[X|Zs]) :- rev(Xs,Ts), rev(Ts,Us), app(Us,Ys,Zs).

:- app2([1,2,3], [1,2,3], Ls).
