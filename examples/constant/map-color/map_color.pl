color_map([], _).
color_map([Region | Regions], Colors) :-
	color_region(Region, Colors),
	color_map(Regions, Colors).

color_region(region(_, Color, Neighbors), Colors) :-
	select(Color, Colors, Colors1),
	subset(Neighbors, Colors1).

select(X, [X | Xs], Xs).
select(X, [Y | Xs], [Y | Zs]) :- select(X, Xs, Zs).

subset([], _).
subset([X | Xs], Ys) :- member(X, Ys), subset(Xs, Ys).

member(X, [X | _]).
member(X, [_ | Xs]) :- member(X, Xs).

map([region(belize, Belize, [Guatemala]),
     region(guatemala, Guatemala, [Belize, El_Salvador, Honduras]),
     region(el_salvador, El_Salvador, [Guatemala, Honduras]),
     region(honduras, Honduras, [Guatemala, El_Salvador, Nicaragua]),
     region(nicaragua, Nicaragua, [Honduras, Costa_rica]),
     region(costa_rica, Costa_rica, [Nicaragua, Panama]),
     region(panama, Panama, [Costa_rica])]).

:- map(X).
