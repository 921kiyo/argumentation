% Examples in the question
myAsm(a).
myAsm(b).
contrary(a, p).
myRule(p, [b]).
myRule(p, []).

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
argument((C, [])):-
	myRule(C, []).

argument((C, [C])):-
	myAsm(C).

% argument((C, Res)):-
%
% 	myRule(C, L),
% 	findall(X, (myRule(C, List), member(X, List)), Lis),
% 	combs(Lis, Res).

combs([],[]).

combs([H|T],[H|T2]) :-
    combs(T,T2).
combs([_|T],T2) :-
    combs(T,T2).


attacks((C1, X1), (C2, X2)):-
	argument((C1, X1)),
	% print(C1),nl,
	contrary(C2, C1),
	myAsm(C2),
	argument(X2, [C2]).
	% myRule(C2, [Attacker|Y]),
	% contrary(Attacker, C1).


% argument((C, List)):-
% 	length(List, Len),
% 	Len > 1,
% 	% check_argument(List),
% 	findall(X, (myAsm(X), member(X, List)), L),
% 	print(L),nl,
% 	myRule(C, List).
%
% % check_argument([]).
% % check_argument([H|T]):-
% % 	myAsm(H),
% % 	check_argument(T).
%
% % When there is only one assumption
% argument((C, [X])):-
% 	myAsm(X),
% 	\+ myRule(C, [X|_]),
% 	X = C.
%
% % If C itself is an assumption
% argument((C, [C])):-
% 	myAsm(C).
%
% argument((C, [])):-
% 	myRule(C, []).
