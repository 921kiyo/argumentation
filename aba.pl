% Examples in the question
% myAsm(a).
% myAsm(b).
% contrary(a, p).
% myRule(p, [b]).
% myRule(p, []).


% LabTS test 3 for argument/1
% LabTS test 1,2 for attack/2
% argument((z, X)).

% myAsm(a).
% myAsm(b).
% myAsm(c).
% myRule(x,[]).
% myRule(y,[b]).
% myRule(z,[a, p]).
% contrary(a, x).
% contrary(b, y).
% contrary(c, z).


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

myAsm(c).
myAsm(d).
myAsm(w).
myRule(b, [c,a]).
myRule(a, [d]).
myRule(p, [w]).
contrary(d, p).

% If C is an assumption, return itself
argument((C, [C])):-
	myAsm(C).

% If C is not an assumption, and support is empty, return empty set.
argument((C, [])):-
	\+(myAsm(C)),
	myRule(C, List),
	length(List, 0).

% X has to be instantiated. If not, this predicate will be ignored
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
% If C is NOT an assumption, check if all supports are legit
argument((C, Res)):-
	\+(myAsm(C)),
	myRule(C, List),
	length(List, Len),
	Len > 0,
	check_support(List),
	get_assumption(List, Res).


check_support2([], _).
check_support2([H|T], C):-
	myRule(C, List),
	member(H, List),
	check_support2(T, C).

not_inst(Var):-
  \+(\+(Var=0)),
  \+(\+(Var=1)).

% Check if all supports are legit
% (including supports of supports, supports of supports of supports ....etc)
check_support([]).

check_support([H|T]):-
	myAsm(H),
	check_support(T).

check_support([H|T]):-
	\+(myAsm(H)),
	myRule(H, List),
  % This predicate go deeper in the supports of supports, and check
   % all dependent supports are legit
	check_support(List),
	check_support(T).

% Among all supports, this predicate collects only assumptions
get_assumption([], []).
get_assumption([H|T1], [H|T2]):-
	myAsm(H),
	get_assumption(T1,T2).

% Does this make sense??
get_assumption([H1|T1], T2):-
	\+(myAsm(H1)),
	member(H1, List),
	get_assumption(C, T1,T2).

attacks((C1, X1), (C2, X2)):-
	argument((C1, X1)),
	argument((C2, X2)),
	contrary(A, C1),
	member(A, X2).

attacks((C1, X1), (C2, X2)):-
	argument((C1, X1)),
	argument((C2, X2)),
	contrary(C1, A),
	member(A, X2).
