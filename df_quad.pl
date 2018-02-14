% arguments

argument(a1).
argument(a2).
argument(a3).
argument(a4).
argument(a5).
argument(a6).
argument(a7).
argument(a8).

% relationships

% supports(a7, a2).
% supports(a8, a4).
%
% attacks(a5,a3).
% attacks(a6,a3).
% attacks(a4,a2).
% attacks(a3,a2).
% attacks(a2,a1).

% Example2
% supports(a4, a2).
% supports(a5, a2).
% supports(a6, a2).

supports(xxxxxx,xxxxxx).
attacks(xxxxxx,xxxxxx).
% attacks(a3,a2).
% attacks(a3,a1).

% Example3
% attacks(a7,a6).
% attacks(a6,a2).
% attacks(a5,a2).
% attacks(a4,a2).
% attacks(a3,a2).
% attacks(a2,a1).




% base scores

base(a1, 0.5).
base(a2, 0.5).
base(a3, 0.5).
base(a4, 0.5).
base(a5, 0.5).
base(a6, 0.5).
base(a7, 0.5).
% base(a8, 0.5).


/*
TODO: Tests/Issues

1. If no attacks/2 is provided
2. If no supports/2 is provided
3. When there are more supporters than attackers
4. Haven't tested different base score

*/

% combination function
comb_func(V0, Va, Vs, C):-
	(Va >= Vs -> C is (V0 - (V0 * abs((Vs - Va)))) ;
	 C is (V0 + ((1 - V0) * (abs((Vs - Va)))))).

% base function to handle sequences of strengths of attackers or supporters
base_func(V1, V2, NewScore):-
	NewScore is (V1 + ((1 - V1)* V2)).

% When Arg has no child, simply returns the base case of Arg
strength(Arg, BS):-
	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
	length(Attackers, 0),
	findall(Supp, (argument(Supp), supports(Supp,Arg)), Supporters),
	length(Supporters, 0),
	base(Arg, BS).

% When Arg has only supporter children and no attackers,
strength(Arg, TotalScore):-
	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
	length(Attackers, 0),
	findall(Supp, (argument(Supp), supports(Supp,Arg)), Supporters),
	length(Supporters, Supp_len),
	Supp_len > 0,
	strength_aggregation(Arg, Supporters, SuppScore),
	base(Arg, BS),
	comb_func(BS, 0, SuppScore, TotalScore).

% When Arg has only attacker children and no supporters
strength(Arg, TotalScore):-
	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
	length(Attackers, Att_len),
	Att_len > 0,
	strength_aggregation(Arg, Attackers, AttScore),
	findall(Supp, (argument(Supp), supports(Supp,Arg)), Supporters),
	length(Supporters, 0),
	base(Arg, BS),
	comb_func(BS, AttScore, 0, TotalScore).

% When Arg has both attacker and supporter children
strength(Arg, TotalScore):-
	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
	length(Attackers, Att_len),
	Att_len > 0,
	strength_aggregation(Arg, Attackers, AttScore),
	findall(Supp, (argument(Supp), supports(Supp,Arg)), Supporters),
	length(Supporters, Supp_len),
	Supp_len > 0,
	strength_aggregation(Arg, Supporters, SuppScore),
	base(Arg, BS),
	comb_func(BS, AttScore, SuppScore, TotalScore).

% Strength aggregation function accumulator
% Children: list of attackers or supporters children associated with Arg.
strength_aggregation(Arg, Children, Score):- strength_aggregation(Arg, Children, Score, []).

% If n = 0: F(S) = 0
strength_aggregation(_, [], 0, ScoreList):-
		length(ScoreList, 0).

% If n = 1: F(S) = v1
strength_aggregation(_, [], Score, ScoreList):-
		length(ScoreList, 1),
		[Score] = ScoreList.

% If n = 2: F(S) = v(v1, v2)
strength_aggregation(_, [], Score, ScoreList):-
		length(ScoreList, 2),
		[Score1, Score2] = ScoreList,
		base_func(Score1, Score2, Score).

% If n > 2: F(S) = 0: F(S) = f(F(v1.... vn-1), vn)
% Take V1 and V2, calculate the score using base function,
% which will be stored in the accumulator
strength_aggregation(Arg, [], Score, [V1, V2|Rest]):-
		length([V1, V2|Rest], ListLen),
		ListLen > 2,
		base_func(V1, V2, NewScore),
		strength_aggregation(Arg, [], Score, [NewScore|Rest]).

% If n > 2: F(S) = 0: F(S) = f(F(v1.... vn-1), vn)
% Calculate nth V
strength_aggregation(Arg, [V1|Rest], S, Acc):-
		strength(V1, TotalScore),
		strength_aggregation(Arg, Rest, S, [TotalScore|Acc]).
