%argument([a,b,c,d,e,f]).
% attacks([(a,b),(b,c),(a,a)]).
%attacks([[a,b],[c,b],[c,d],[d,c], [d,e], [e,e]]).

argument([a,b,c,d,e,f,g,h,i,j,k,l,m,p]).
attacks([[h,e],[e,b],[d,e],[d,b],[d,a],[h,a],[h,p],[p,q],[n,p],[n,f],
	 [i,n],[j,n],[i,j],[j,i],[i,e],[p,c],[p,d],[c,a],[p,l],[l,m],
	 [m,k],[k,l],[m,c],[g,d],[g,p]]).


conflict_free([]).

conflict_free(C):- 
	check_alive(C), 
	findall(Res ,perm(C, Res), List),
        check_conflict(C, List).


check_alive([]).

check_alive([H|T]):- 
	argument(List), 
	member(H, List), 
	check_alive(T).


check_conflict(_, []).

check_conflict(C, [H|T]):-
	attacks(List),
	\+(member(H, List)), 
	check_conflict(C, T).


perm([A], [A,A]).

perm(List, Result):- 
	length(List, Len), 
	Len > 1, 
	perm_acc(List, Result, 2).

perm_acc(_, [], 0).

perm_acc(List,[H|Perm], C):-
	C > 0,
	NewC is C -1,
	delete(H,List,Rest),
	perm_acc(Rest,Perm,NewC).

perm(List, [X,X]):- 
	length(List, Len), 
	Len > 1, 
	member(X, List).

delete(X,[X|T],T).

delete(X,[H|T],[H|NT]):-
	delete(X,T,NT).


% admiss(C): suceeds if C is an admissible (set of) argument(s).

admiss(C):-
	conflict_free(C),
        attackers_list(C, Attackers),
        findall(X, (attackers_list(Attackers, Defenders), member(X, Defenders), member(X, C)), Counters),
	length(Attackers, Att_len),
	length(Counters, Counter_len),
	Counter_len >= Att_len.

% attackers_list(Def, Att): where Att is a list of the arguments which are attacking 
% elements of the list, Def (defendents). Uses auxillary predicate 
% get_attackers(Def, Att, Acc): which recurses through the defenders and accumulates
% the attackers for each defendent.


% what if setof fails, what if there is no attackers etc.

attackers_list(Def, Att):- 
	get_attackers(Def, Att, []).

get_attackers([], Acc, Acc).

get_attackers([H|T], Att, Acc):-
	attacks(Attackers),
	findall(X, member([X, H], Attackers), L),
	append(Acc, L, NewAcc),
	get_attackers(T, Att, NewAcc).

grounded(C):-
	attacks(Attackers),
	argument(Args),	
	findall(X, (member(X, Args), attackers_list([X], Att), length(Att, Att_len), Att_len == 0), Not_attacked),
	recurse(Not_attacked, Grounded_exts),
	append(Not_attacked, Grounded_exts, C).	

recurse(Not_attacked, Grounded_exts):-
	recurse(Not_attacked, Grounded_exts, [], [],X).

recurse([], Grounded_exts, Grounded_exts, A, A).

recurse([H|T], Grounded_exts, T_in, T_out, Z):-
	attacks(Attackers),
	argument(Args),
	findall(Attacked_by, member([H, Attacked_by], Attackers), Out),
	findall(I, (member(Attacked_by, Out), member([Attacked_by, I], Attackers)), In),
	append(T_out, Out, New_out),
	update_in(In, New_out, New_in),	
	recurse(T, Grounded_exts, New_in, New_out, Z).

update_in(In, New_out, New_in):-
	update_in(In, New_out, New_in, []).

update_in([], _, New_in, New_in).

update_in([H|T], Out, X, T2):-
	attackers_list([H], Attackers),
	sort(Attackers, Attackers_sort),
	(
	 setof(Att, (member(Att, Attackers_sort), member(Att, Out)), Attackers2),
	 \+(member(H, Out)), append(Attackers_sort, [], Attackers2) -> NewT2 = [H|T2] ;
	NewT2 = T2),
	update_in(T, Out, X, NewT2).



% Edge cases
% Singleton_argument
% Should conflict_free(X) return a set of conflict-free??
% conflict_free([c,f]) when f doesn't exist
% conflict_free([a,a]) -> conflict_free(a)
% perm([1,2,3,1], X).
