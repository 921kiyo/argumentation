% argument(a).
% argument(b).
% argument(c).
% argument(d).
% argument(e).
% argument(f).
%
% attacks(b,a).
% attacks(d,c).
% attacks(c,a).
% attacks(e,d).
% attacks(f,b).

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


% grounded(C):-
%   argument(C),
%   check_ground(C).
%
% check_ground(C):-
%   argument(C),
%   \+ (attacks(X, C), grounded(X)).
%
% check_ground(C):-



% grounded(C):-
%   argument(C),
%   \+ (attacks(X, C), grounded(X)).

grounded(C):-
  argument(C),
  attacks(X, C), attacks(C, X).

grounded(C):-
  argument(C),
  findall(A,(attacks(A, C), \+(grounded(A))), Undicided_List),
  print(Undicided_List),
  check_attack_back(C, G_List).

check_attack_back(C,[]).
check_attack_back(C,[H|T]):-
  attacks(C, H),
  check_attack_back(C, T).
