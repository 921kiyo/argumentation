% argument([a,b,c,d,e,f,g,h,i,j,k,l,m,p]).
% attacks([[h,e],[e,b],[d,e],[d,b],[d,a],[h,a],[h,p],[p,q],[n,p],[n,f],
% 	 [i,n],[j,n],[i,j],[j,i],[i,e],[p,c],[p,d],[c,a],[p,l],[l,m],
% 	 [m,k],[k,l],[m,c],[g,d],[g,p]]).

% argument(a).
% argument(b).
% argument(c).
% argument(d).
% argument(e).
%
% attacks(a,b).
% attacks(c,b).
% attacks(c,d).
% attacks(d,c).
% attacks(d,e).
% attacks(e,e).

attackers_list(Def, Att):-
	get_attackers(Def, Att, []).

get_attackers([], Acc, Acc).

get_attackers([H|T], Att, Acc):-
	findall([X,Y], attacks(X,Y), Attackers),
	findall(X, member([X, H], Attackers), L),
	append(Acc, L, NewAcc),
	get_attackers(T, Att, NewAcc).

grounded(C):-
	findall([X,Y], attacks(X,Y), Attackers),
	findall(X, argument(X), Args),
	findall(X, (member(X, Args), attackers_list([X], Att), length(Att, Att_len), Att_len == 0), Not_attacked),
	recurse(Not_attacked, Grounded_exts),
	append(Not_attacked, Grounded_exts, List),
	member(C, List).

recurse(Not_attacked, Grounded_exts):-
	recurse(Not_attacked, Grounded_exts, [], [],X).

recurse([], Grounded_exts, Grounded_exts, A, A).

recurse([H|T], Grounded_exts, T_in, T_out, Z):-
	findall([X,Y], attacks(X,Y), Attackers),
	findall(X, argument(X), Args),
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
