% arguments

argument(a1).
argument(a2).
argument(a3).
argument(a4).
argument(a5).
argument(a6).
% argument(b).

% relationships

% attacks(a2, a1).
% attacks(b, a1).
%
% supports(a3, a1).

attacks(a5,a3).
attacks(a6,a3).
attacks(a4,a2).
attacks(a3,a2).
attacks(a2,a1).

% base scores

base(a1, 0.5).
base(a2, 0.5).
base(a3, 0.5).
base(a4, 0.5).
base(a5, 0.5).
base(a6, 0.5).
% base(a5, 0.5).
% base(b, 0.5).


/*
comb_func(V0, Va, Vs, C):-
	(Va >= Vs -> C is (V0 - (V0 * abs((Vs - Va)))) ;
	 C is (V0 + ((1 - V0) * (abs((Vs - Va)))))).
*/

comb_func(V0, Va, Vs, C):-
	Va >= Vs,
	C is (V0 - (V0 * abs((Vs - Va)))).


comb_func(V0, Va, Vs, C):-
	Va < Vs,
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

strength(Arg, BS):-
  % I think this negation is causing multiple answers issue....
	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
	length(Attackers, 0),
	base(Arg, BS),
	argument(Att).

strength(Arg, TotalScore):-
  % This is causing multiple answers
	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
	length(Attackers, 1),
	print(Arg),nl,
	print(Attackers),nl,
	strength_aggregation(Arg, Attackers, Score),
	print(Score),nl,
	base(Arg, BS),
	comb_func(BS, Score, 0, TotalScore).

strength(Arg, TotalScore):-
  % This is causing multiple answers
	attacks(A, Arg),
	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
	length(Attackers, Att_len),
	Att_len > 1,
	strength_aggregation(Arg, Attackers, Score),
	base(Arg, BS),
	[Att1, Att2] = Attackers,
	comb_func(BS, Score, 0, TotalScore).

strength_aggregation(Arg, Attackers, Score):- strength_aggregation(Arg, Attackers, Score, []).

strength_aggregation(Arg, [], 0, ScoreList):-
		length(ScoreList, 0).

strength_aggregation(Arg, [], Score, ScoreList):-
		length(ScoreList, 1),
		[Score] = ScoreList.

strength_aggregation(Arg, [], Score, ScoreList):-
		length(ScoreList, 2),
		[Score1, Score2] = ScoreList,
		base_func(Score1, Score2, Score).

strength_aggregation(Arg, [], Score, [Score1, Score2|Rest]):-
		length([Score1, Score2|Rest], ListLen),
		ListLen > 2,
		base_func(Score1, Score2, NewScore),
		strength_aggregation(Arg, [], Score, [NewScore|Rest]).

strength_aggregation(Arg, [], 0, ScoreList):-
		length(ScoreList, 0).

strength_aggregation(Arg, [Attacker|Rest], S, Acc):-
		strength(Attacker, Score),
		strength_aggregation(Arg, Rest, S, [Score|Acc]).
