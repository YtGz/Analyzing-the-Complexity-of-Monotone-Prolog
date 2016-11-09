balance(T,TB) :-
	balance(T,I-[],[(TB,I-[])|X]-X,Rest-[],Rest-[]).

balance(nil,X-X,A-B,A-B,[(nil,C-C)|T]-T).
balance(tree(L,V,R),IH-IT,[(tree(LB,VB,RB),A-D)|H]-[(LB,A-[VB|X]),(RB,X-D)|T],HR-TR,NH-NT) :-
        balance(L,IH-[V|IT1],H-T,HR1-TR1,NH-NT1),
        balance(R,IT1-IT,HR1-TR1,HR-TR,NT1-NT).

:- balance(tree(nil,2,tree(nil,3,tree(nil,4,nil))),X).
