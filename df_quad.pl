% arguments

argument(a1).
argument(a2).
argument(a3).
argument(a4).
argument(a5).
argument(a6).
% Uncomment this if you want to include a supporter
% argument(a7).

% relationships

% If you uncomment this, DF Score should be 0.265625
% If you don't include this, DF score should be 0.390625
% supports(a7, a2).

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
% Uncomment this if you want to include a supporter
% base(a7, 0.5).


/*
TODO: Check edge cases

1. If no attacks/2 is provided
2. If no supports/2 is provided
3. When there are more supporters than attackers
4. If you include an argument and do not add relationship(attacks, supporters), this doesn't work
4. Haven't tested different base score
5. Very complicated trees (we can do this using http://www.arganddec.com/)
*/

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

base_func(V1, V2, NewScore):-
	NewScore is (V1 + ((1 - V1)* V2)).


/* This works when there is only attackers

strength(Arg, BS):-
	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
	length(Attackers, 0),
	base(Arg, BS).
strength(Arg, TotalScore):-
	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
	length(Attackers, Att_len),
	Att_len > 0,
	strength_aggregation(Arg, Attackers, Score),
	base(Arg, BS),
	comb_func(BS, Score, 0, TotalScore).

*/


% When Arg has no child
strength(Arg, BS):-
	findall(Att, (argument(Att), attacks(Att,Arg)), Attackers),
	length(Attackers, 0),
	findall(Supp, (argument(Supp), supports(Supp,Arg)), Supporters),
	length(Supporters, 0),
	base(Arg, BS).

% When Arg has only supporter children and no attackers
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
	comb_func(BS, AttScore, Supp, TotalScore).

% Strength aggregation function accumulator
% Children: list of attackers or supporters children associated with Arg.
strength_aggregation(Arg, Children, Score):- strength_aggregation(Arg, Children, Score, []).

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
    % TODO This is where the recursion takes place, but it doesn't work when multiple attackers and supporters are provided.
		strength(Attacker, Score),
		strength_aggregation(Arg, Rest, S, [Score|Acc]).
