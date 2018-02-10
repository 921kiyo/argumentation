% Examples in the question
myAsm(a).
myAsm(b).
contrary(a, p).
myRule(p, [b]).
myRule(p, []).


% LabTS test 3
% argument((z, X)).
% myAsm(a).
% myAsm(b).
% myAsm(c).
% myRule(x,[]).
% myRule(y,[b]).
% myRule(z,[a, p]).

% Examples in the paper
% myAsm(a).
% myAsm(b).
% myAsm(c).
% contrary(a, r).
% contrary(b, s).
% contrary(c, t).
% myRule(p, [q,a]).
% myRule(q, []).
% myRule(r, [b,c]).
% argument((C, [])):-
% 	myRule(C, []).

% If C is an assumption, return itself
argument((C, [C])):-
	myAsm(C).

% If C is NOT an assumption, check all supports
argument((C, Res)):-
	\+(myAsm(C)),
	myRule(C, List),
	length(List, Len),
	Len > 0,
	check_support(List),
	get_assumption(List, Res).

get_assumption([], []).
get_assumption([H|T1], [H|T2]):-
	myAsm(H),
	get_assumption(T1,T2).

get_assumption([H1|T1], T2):-
	\+(myAsm(H1)),
	member(H1, List),
	get_assumption(C, T1,T2).

check_support([]).

check_support([H|T]):-
	myAsm(H),
	check_support(T).

check_support([H|T]):-
	\+(myAsm(H)),
	myRule(H, List),
	check_support(List),
	check_support(T).

% If C is not an assumption, and empty support, return empty set.
argument((C, [])):-
	\+(myAsm(C)),
	myRule(C, List),
	length(List, 0).

% % X has to be instantiated
argument((C, X)):-
	\+(not_inst(X)),
	check_support2(X, C),
	myRule(C, X).

check_support2([], C).
check_support2([H|T], C):-
	myRule(C, List),
	member(H, List),
	check_support2(T, C).

not_inst(Var):-
  \+(\+(Var=0)),
  \+(\+(Var=1)).

attacks((C1, X1), (C2, X2)):-
	argument((C1, X1)),
	% print(C1),nl,
	contrary(C2, C1),
	myAsm(C2),
	argument((X2, [C2])).
	% myRule(C2, [Attacker|Y]),
	% contrary(Attacker, C1).
