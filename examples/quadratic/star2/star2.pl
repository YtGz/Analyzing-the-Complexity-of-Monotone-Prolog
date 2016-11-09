% Author: Peter Schneider-Kamp
% terminating

%complexity: star(i,i).
star(_,[]).
star([X|U],[X|W]) :- app(U,V,W), star([X|U],W).
app([],L,L).
app([X|L],M,[X|N]) :- app(L,M,N).

:- star([],[]).
