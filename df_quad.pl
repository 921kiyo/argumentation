% arguments

argument(a1).
argument(a2).
argument(a3).
argument(a4).
% argument(a5).
% argument(b).

% relationships

% attacks(a2, a1).
% attacks(b, a1).
%
% supports(a3, a1).

% attacks(a5,a4).
attacks(a4,a2).
attacks(a3,a2).
attacks(a2,a1).

% base scores

base(a1, 0.5).
base(a2, 0.5).
base(a3, 0.5).
base(a4, 0.5).
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


% strength(Arg, BS):-
% 	% findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
%   % I think this negation is causing multiple answers issue....
% 	\+(attacks(Att, Arg)),
% 	base(Arg, BS),
% 	argument(Att).
% 	% length(Attackers, Attck_length),
% 	% Attck_length == 0.
%
% strength(Arg, TotalScore):-
% 	base(Arg, BS),
% 	argument(Att),
% 	attacks(Att, Arg),
% 	strength(Att, Score),!,
% 	comb_func(BS, Score, 0, TotalScore).

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

strength_aggregation(Arg, [], 0, ScoreList):-
		length(ScoreList, 0).


strength_aggregation(Arg, [Attacker|Rest], S, Acc):-
		strength(Attacker, Score),
		% base(Attacker, Score),
		strength_aggregation(Arg, Rest, S, [Score|Acc]).


get_sum(ScoreList, Score):- get_sum(ScoreList, Score, 0).

get_sum([], Score, Score).

get_sum([H|T], S, Acc):-
	NewAcc is Acc + H,
	get_sum(T, S, NewAcc).


% strength_aggregation(Arg, 0):-
% 	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
% 	length(Attackers, 0).
%
% strength_aggregation(Arg, Score):-
% 	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
% 	length(Attackers, 1),
% 	member(A, Attackers),
% 	strength(A, Score),
% 	print(score1), print(Score),nl.
%
% strength_aggregation(Arg, Score):-
% 	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
% 	% print(Attackers),nl,
% 	length(Attackers, Attackers_length),
% 	Attackers_length > 1,
% 	[Attacker1, Attacker2] = Attackers,
% 	strength(Attacker1, Score1),
% 	strength(Attacker2, Score2),
% 	base_func(Score1, Score2, Score),
% 	% print(Attacker1), print(': '), print(Score1),nl,
% 	% print(Attacker2), print(': '), print(Score2),nl,
% 	print(score2),print(Score),nl.


% strength_agg(Arg, BS):-
% 	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
% 	length(Attackers, 0)
% 	base(Arg, BS),
%
% strength_agg(Arg, Score):-
% 	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
% 	length(Attackers, 1),
% 	member(A, Attackers),
% 	strength(A, Score).
%
% strength_agg(Arg, Score):-
% 	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
% 	length(Attackers, Attackers_length),
% 	Attackers_length == 2,
% 	[Att1, Att2] = Attackers,
% 	strength(Att1, Score1),



% strength(Arg, BS):-
% 	% findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
% 	argument(Att),
% 	\+(attacks(Att, Arg)),
% 	base(Arg, BS).
% 	% length(Attackers, Attck_length),
% 	% Attck_length == 0.
%
% strength(Arg, TotalScore):-
% 	base(Arg, BS),
% 	argument(Att),
% 	attacks(Att, Arg),
% 	strength(Att, Score),
% 	TotalScore is (BS - (BS * Score)).
	% comb_func(BS, Score, 0, TotalScore).


% When there is no attacker attacking me
recurse(Arg, BS):-
	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
	length(Attackers, Attck_length),
	Attck_length == 0,
	base(Arg, BS).
	% comb_func(BS, 0, 0, NewScore).

% When there is one attacker attacking me
recurse(Arg, BS):-
	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
	length(Attackers, Attck_length),
	Attck_length == 1,
	member(A, Attackers),
	base(A, BS).

recurse(Arg, Child_Score):-
	base(Arg, BS),
	comb_func(BS, Child_Score ,0, NewScore),
	argument(Att), attacks(Att, Arg),
	recurse(Att, NewScore).

% strength(Arg, FinalScore):-
% 	base(Arg, BS),
% 	recurse(Arg, Attackers_Score, []),
% 	print(fdsa), print(Attackers_Score),
% 	comb_func(BS, Attackers_Score, 0, FinalScore).
%
% recurse(Arg, Score, _):-
% 	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
% 	length(Attackers, Attck_length),
% 	Attck_length == 0,
% 	Score is 0.
%
% recurse(Arg, Score, _):-
% 	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
% 	length(Attackers, Attck_length),
% 	Attck_length == 1,
% 	member(A, Attackers),
% 	base(A, Score).
%
% recurse(Arg, Score, _):-
% 	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
% 	length(Attackers, Attck_length),
% 	Attck_length == 2,
% 	[Attacker1, Attacker2] = Attackers,
% 	base(Attacker1, Score1),
% 	base(Attacker2, Score2),
% 	base_func(Score1, Score2, Score),
% 	member(A, Attackers).
%
% recurse(Arg, Score, Acc):-
% 	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
% 	length(Attackers, Attck_length),
% 	Attck_length > 2,
% 	findall(Att_Score, (argument(Att), attacks(Att,Arg), base(Arg, Att_Score)), Attackers_Score),
% 	go_in_twos(Attackers_Score, Acc),
% 	Score is Acc.
%
% go_in_twos([Final_score], Final_score).
%
% go_in_twos([Att_one, Att_two|T], Acc):-
% 	base_func(Att_one, Att_two, Score),
% 	go_in_twos([Score|T], Acc).







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
