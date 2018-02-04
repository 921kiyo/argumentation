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

supports(a7, a2).
supports(a8, a4).

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
base(a7, 0.5).
base(a8, 0.5).


/*
TODO: Tests/Issues

1. If no attacks/2 is provided
2. If no supports/2 is provided
3. When there are more supporters than attackers
4. Haven't tested different base score
5. Very complicated trees (we can do this using http://www.arganddec.com/)

TODO
6: refactoring
7: Comments

TODO: Minor issues

1. If you look at strength predicate, there are lots of redundant code, so it might be worth
   considering some refactoring (e.g conditional steatement etc).
2. If you trace it, it goes through many steps (800 steps), mainly because of findall
   and non-tail recursion. While the description tells us to avoid "obvious inefficiency",
	 we could ask them whey they mean by that.
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
	comb_func(BS, AttScore, SuppScore, TotalScore).

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
