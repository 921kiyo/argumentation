% arguments

argument(a1).
argument(a2).
argument(a3).
argument(b).

% relationships

attacks(a2, a1).
attacks(b, a1).

supports(a3, a1).

% base scores

base(a1, 0.5).
base(a2, 0.5).
base(a3, 0.5).
base(b, 0.5).


/*
comb_func(V0, Va, Vs, C):-
	(Va >= Vs -> C is (V0 - (V0 * abs((Vs - Va)))) ;
	 C is (V0 + ((1 - V0) * (abs((Vs - Va)))))).
*/

comb_func(V0, Va, Vs, C):-
	Va =< Vs,
	%print(Va),nl,
	%print(Vs),nl,
	C is (V0 - (V0 * abs((Vs - Va)))).

comb_func(V0, Va, Vs, C):-
	Va > Vs,
	C is (V0 + ((1 - V0) * (abs((Vs - Va))))).

/*
aggreg_func(0, [], 0).

aggreg_func(1, [Score], Score).

aggreg_func(2, [Score1, Score2], Score):-
	base_func(Score1, Score2, Score).


aggreg_func(N, [Score1, Score2|T], Score):-
	N > 2,
	N1 is N - 1,
	base_func(Score1, Score2, NewScore),
	append([NewScore], T, NewList),
        aggreg_func(N1,NewList, Temp),
	Score is Temp.

*/

base_func(V1, V2, NewScore):-
	NewScore is (V1 + ((1 - V1)* V2)).

strength(Arg, FinalScore):-
	base(Arg, BS),
	recurse(Arg, Attackers_Score),
	print(fdsa), print(Attackers_Score),
	comb_func(BS, Attackers_Score, 0, FinalScore).

recurse(Arg, Score):-
	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
	length(Attackers, Attck_length),
	Attck_length == 0,
	Score is 0.

recurse(Arg, Score):-
	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
	length(Attackers, Attck_length),
	Attck_length == 1,
	member(A, Attackers),
	base(A, Score).

recurse(Arg, Score):-
	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
	length(Attackers, Attck_length),
	Attck_length == 2,
	[Attacker1, Attacker2] = Attackers,
	base(Attacker1, Score1),
	base(Attacker2, Score2),
	base_func(Score1, Score2, Score),	
	member(A, Attackers).


recurse(Arg, Score):-
	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
	length(Attackers, Attck_length),
	Attck_length > 2,
	member(A, Attackers),
	print(score), print(Score),nl,
	N > 2,
	N1 is N - 1,
	base_func(Score1, Score2, NewScore),
	append([NewScore], T, NewList),
        aggreg_func(N1,NewList, Temp),
	Score is Temp.
	recurse(A, AttackScore),
	print(Attackers_Score),nl,
	aggreg_func(Attck_length, Attackers_Score, AttackScore).


/*

recurse(Arg, Score):-
	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
	length(Attackers, Attck_length),
	Attck_length > 2,
	member(A, Attackers),
	print(score), print(Score),nl,
	N > 2,
	N1 is N - 1,
	base_func(Score1, Score2, NewScore),
	append([NewScore], T, NewList),
        aggreg_func(N1,NewList, Temp),
	Score is Temp.
	recurse(A, AttackScore),
	print(Attackers_Score),nl,
	aggreg_func(Attck_length, Attackers_Score, AttackScore).


*/



/*
strength(Arg, AS):-
	base(Arg, BS),
	findall(Att, (argument(Att), attacks(Att,Arg), base(Arg, Att_Score)), Attackers),
	findall(Att_Score, (argument(Att), attacks(Att,Arg), base(Arg, Att_Score)), Attackers_Score),
	length(Attackers_Score, Att_length),
	Att_length == 0,
	aggreg_func(Att_length, Attackers, AS).
*/




/*
strength(Arg, FinalScore):-
	base(Arg, BS),
	findall(Att, (argument(Att), attacks(Att,Arg), base(Arg, Att_Score)), Attackers),
	print(Attackers),nl,
	findall(Att_Score, (argument(Att), attacks(Att,Arg), base(Arg, Att_Score)), Attackers_Score),
	length(Attackers, Attck_length),
	Attck_length > 0,
	member(A, Attackers),
	strength(A, AttackScore),
	aggreg_func(Attck_length, Attackers_Scores, Attack_total_score),

	findall(Supp, (argument(Supp), supports(Supp,Arg), base(Arg, Supp_Score)), Supporters),
	findall(Supp_Score, (argument(Supp), supports(Supp,Arg), base(Arg, Supp_Score)), Supporters_Score),
	length(Supporters, Supp_length),
	Supp_length > 0,
	member(S, Supporters),
	strength(S, Supporters),
	aggreg_func(Supp_length, Supporters_Scores, Supp_total_score),
		
	comb_func(BS, Attack_total_score, Supp_total_score, FinalScore).

	
*/