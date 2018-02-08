myAsm(a).
myAsm(b).
contrary(a, p).
myRule(p, [b]).
myRule(p, []).

argument((C, [X|Y])):-
	myAsm(X),
	myRule(C, [X|Y]).

argument((C, [X])):-
	myAsm(X),
	\+ myRule(C, [X|_]),
	X = C.

argument((C, [])):-
	myRule(C, []).

attacks((C1, X1), (C2, X2)):-
	argument((C1, X1)),
	myRule(C2, [X|Y]),
	Attacker = X,
	contrary(Attacker, C1).
	