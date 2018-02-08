myAsm(a).
myAsm(b).
contrary(a, p).
myRule(p, [b]).
myRule(p, []).

argument((C, [X|Y])):-
	myAsm(X),
	myRule(C, [X|Y]).

argument((C, [])):-
	myRule(C, []).

