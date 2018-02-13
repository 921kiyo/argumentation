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
% myRule(p,[b]).
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

% This should give 6 argument
myAsm(c).
myAsm(d).
myAsm(w).
myRule(b,[c,a]).
myRule(a,[d]).
myRule(p,[w]).
contrary(d, p).
contrary(c, z).
contrary(w, v).

% Example in the slide
% myAsm(a).
% myAsm(b).
% myAsm(c).
% myRule(p,[q,a]).
% myRule(q,[]).
% myRule(r,[b,c]).
% contrary(a, r).
% contrary(b, s).
% contrary(c, t).


% myAsm(c).
% myAsm(d).
% myAsm(w).
% myRule(b, [c,a]).
% myRule(a, [d]).
% myRule(p, [w]).
% contrary(d, p).

% If C is an assumption, return itself
argument((C, [C])):-
	myAsm(C).

% If C is not an assumption, and support is empty, return empty set.
argument((C, [])):-
	myRule(C, []),
	\+(myAsm(C)).

% If C is NOT an assumption, check if all supports are legit
argument((C, Assumptions)):-
	myRule(C, List),
	length(List, Len),
	Len > 0,
	check_support(List),
	get_assumption(List, Assumptions).

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

get_assumption([H1|T1], NewT2):-
	\+(myAsm(H1)),
	myRule(H1, List),
	get_assumption(List, NewArg),
	append(NewArg, T2, NewT2),
	get_assumption(T1,T2).

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
