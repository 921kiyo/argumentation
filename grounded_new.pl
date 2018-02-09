/*
argument(a).
argument(b).
argument(c).
argument(d).
argument(e).
argument(f).
argument(g).
argument(h).
argument(i).
argument(j).
argument(k).
argument(l).
argument(m).
argument(n).
argument(p).
argument(q).
    
attacks(h,e).
attacks(e,b).
attacks(d,e).
attacks(d,b).
attacks(d,a).
attacks(h,a).
attacks(h,p).
attacks(p,q).
attacks(n,p).
attacks(n,f).
attacks(i,n).
attacks(j,n).
attacks(n,p).
attacks(n,f).
attacks(i,j).
attacks(j,i).
attacks(i,e).
attacks(p,c).
attacks(c,a).
attacks(p,l).
attacks(m,k).
attacks(l,m).
attacks(k,l).
attacks(m,c).                                                                  
attacks(g,d).
attacks(g,p).
*/
/*
argument(a).
argument(b).
argument(c).
argument(d).
argument(e).
argument(f).

attacks(b,a).
attacks(d,c).
attacks(c,a).
attacks(e,d).
attacks(f,b).
*/


% forall(X, Y), meaning there is no instantiation of X for which Y is false
forall(X, Y):- 
	\+ (X, \+ Y).
	
grounded(A):- 
	argument(A),
	forall(attacks(B, A), (attacks(C, B), B \== C, A \== C, check_loops(B, C), grounded(C))).

check_loops(B, C):-
	findall(X, attacks(B, X), Attacked_by_B),
	findall(X, attacks(X, C), Attacking_C),
	\+ (member(X, Attacked_by_B), member(X, Attacking_C)),
	(\+ attacks(B, C) ; 
	    \+ (member(X, Attacked_by_B), member(Y, Attacking_C), attacks(X, Y))).