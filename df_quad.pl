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



comb_func(V0, Va, Vs, C):-
	(Va >= Vs -> C is (V0 - (V0 * abs((Vs - Va)))) ;
	 C is (V0 + ((1 - V0) * (abs((Vs - Va)))))).

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

base_func(V1, V2, NewScore):-
	NewScore = V1 + (1-V1)*V2.

strength(Arg, FinalScore):-
	base(Arg, BS),
	findall(Att_Score, (argument(X), attacks(X,Arg), base(Arg, Att_Score)), Att_Scores),
	length(Att_Scores, Attck_length),
	findall(Supp_Score, (argument(X), supports(X,Arg), base(Arg, Supp_Score)), Supp_Scores),
	length(Supp_Scores, Supp_length),
	aggreg_func(Attck_length, Att_Scores, Attack_total_score),
	aggreg_func(Supp_length, Supp_Scores, Supp_total_score),
	comb_func(BS, Attack_total_score, Supp_total_score, FinalScore),
	print(FinalScore).

