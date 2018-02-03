base(a, 1).
base(b, 1).
base(c, 1).
base(d, 1).
base(e, 1).

attacks(e, d).
attacks(d, c).
attacks(c, b).
attacks(b, a).

get_value(X, V):-
  \+attacks(A,X),
  base(X, V).

get_value(X, TotalScore):-
  attacks(Attacker,X),
  get_value(Attacker, Score),
  TotalScore is Score + 1.
