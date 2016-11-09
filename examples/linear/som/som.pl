som3([],Bs,Bs).
som3(As,[],As).
som3([A|As],[B|Bs],[A+B|Cs]) :-
 som3(As,Bs,Cs).

:- som3([1,2,3],Ys,Zs).
