/* Implementation of grounded(X). Note this predicate can only be used to
   test whether a given argument (constant) is a member of the grounded extension, i.e. it
   will not return members of the grounded extension if tested with an unground variable */

% pre-defined predicate forall(X, Y), meaning there is no instantiation of X for which Y is false
forall(X, Y):- 
	\+ (X, \+ Y).
	
% Grounded(A): where A is a member of the grounded extension
grounded(A):- 
	argument(A),
	forall(attacks(B, A),
	       (argument(B), argument(C), attacks(C, B), B \== C, A \== C, check_loops(B, C), grounded(C))).

% Second case of Grounded(A): [] is the grounded extension if there are no unattacked arguments
grounded(A):-
	A == [],
	findall(X, argument(X), Args),
	findall(X, (member(X, Args), attackers_list([X], Att), length(Att, 0)), Unattacked),
	Unattacked == [].

% auxillary predicate attackers_list(Def, Att): which returns a list of attackers (Att) which are
% attacking Def (uses get_attackers(Def, Att, Acc) predicate to accumulate these attackers).
attackers_list(Def, Att):-
	get_attackers(Def, Att, []).

get_attackers([], Acc, Acc).

get_attackers([H|T], Att, Acc):-
	findall([X,Y], attacks(X,Y), Attackers),
	findall(X, member([X, H], Attackers), L),
	append(Acc, L, NewAcc),
	
get_attackers(T, Att, NewAcc).

% auxillary predicate check_loops(B, C): checks for (undecided) loops between arguments B and C
% when confirming a member in the grounded extension
check_loops(B, C):-
	findall(X, attacks(B, X), Attacked_by_B),
	findall(X, attacks(X, C), Attacking_C),
	\+ (member(X, Attacked_by_B), member(X, Attacking_C)),
	(\+ attacks(B, C) ; 
	    \+ (member(X, Attacked_by_B), member(Y, Attacking_C), attacks(X, Y))).
