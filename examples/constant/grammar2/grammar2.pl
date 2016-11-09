% Simple sentence parser.  The parse tree of a sentence is s(Num,Noun,Verb)
% where Num is 's' for singular or 'p' for plural.

% This program should be run to see how well we're doing with nonground
% structures.  The noun phrase recognizer returns a partial structure with an
% unbound variable for the verb; the verb phrase recognizer succeeds only
% if the verb agrees in number with the determiner and noun.

% The top level call has an unbound variable for the input sentence; since
% this is a finite grammar, this call returns all valid sentences and
% their parse trees.  A sample output:

% S = s(p,book,fall),
% S1 = [the,books,fall|X],	(i.e. "the books fall" is a good start
% S2 = [fall|X], 		 to a sentence...)
% S3 = X.


parse(S0,Meaning) :- np(S0,S1,Meaning) , verb(S1,[],Meaning).
parse(S0,Meaning) :- np(S0,S1,Meaning) , verb(S1,S2,Meaning) , S2 = [].

% The grammar:

np(Si,So,S) :- det(Si,St,T) , noun(St,So,N) , comb(T,N,S).

comb(a,N-s,s(s,N,V)).	% a sentence with determiner "a" must be singular
comb(the,N-P,s(P,N,V)).	% with determiner "the" it can be plural or singular

det([a|S],S,a).
det([the|S],S,the).

noun([book|S],S,book-s).
noun([books|S],S,book-p).
noun([box|S],S,box-s).
noun([boxes|S],S,box-p).

verb([falls|So],So,s(s,N,fall)).
verb([fall|So], So,s(p,N,fall)).
verb([flies|So],So,s(s,N,fly)).
verb([fly|So],  So,s(p,N,fly)).

:- parse([the,boxes,fly],S).
