
% Complicated version
/*
argument(a).
argument(b).
argument(c).
argument(d).
argument(e).
argument(f).
argument(g).
argument(h).
argument(i).
argument(j).
argument(k).
argument(l).
argument(m).
argument(n).
argument(p).

attacks(h,e).
attacks(e,b).
attacks(d,e).
attacks(d,b).
attacks(d,a).
attacks(h,a).
attacks(h,p).
attacks(p,q).
attacks(n,p).
attacks(n,f).
attacks(i,n).
attacks(j,n).
attacks(n,p).
attacks(n,f).
attacks(i,j).
attacks(j,i).
attacks(i,e).
attacks(p,c).
attacks(p,d).
attacks(c,a).
attacks(p,l).
attacks(l,m).
attacks(m,k).
attacks(k,l).
attacks(m,c).
attacks(g,d).
attacks(g,p).
*/

% Easy version
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

% failed on LabTS
argument(a).
argument(b).
argument(c).
argument(d).
argument(e).
argument(f).

attacks(b,a).
attacks(d,c).
attacks(c,a).
attacks(e,d).
attacks(f,b).

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
	print(Grounded_exts),nl.
	append(Not_attacked, Grounded_exts, List),
	member(C, List).

recurse(Not_attacked, Grounded_exts):-
	recurse(Not_attacked, Grounded_exts, [], [],X).

recurse([], Grounded_exts, Grounded_exts, A, A).

% This doesn't make sense, because [H|T] is only the nodes that are not being attacked,
% But surely we want to do recursion more than just the number of these nodes right?
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
