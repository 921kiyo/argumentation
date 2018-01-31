argument([a,b,c,d,e,f]).
% attacks([(a,b),(b,c),(a,a)]).
attacks([[a,b],[c,b],[c,d],[d,c], [d,e], [e,e]]).

conflict_free(C):- check_alive(C), findall(Res ,perm(C, Res), List),
                   check_conflict(C, List).
conflict_free([]).

check_alive([]).
check_alive([H|T]):- argument(List), member(H, List), check_alive(T).

check_conflict(_, []).
check_conflict(C, [H|T]):-
  attacks(List),
  \+(member(H, List)), check_conflict(C, T).


perm([A], [A,A]).
perm(List, Result):- length(List, Len), Len > 1, perm_acc(List, Result, 2).
perm_acc(List, [], 0).

perm_acc(List,[H|Perm], C):-
  C > 0,
  NewC is C -1,
  NewC is C -1, delete(H,List,Rest),perm_acc(Rest,Perm,NewC).

perm(List, [X,X]):- length(List, Len), Len > 1, member(X, List).

delete(X,[X|T],T).
delete(X,[H|T],[H|NT]):-delete(X,T,NT).


admissible(C):- conflict_free(C),
                findall(Res , attacking_list(C, Res), List),
                % print(List),nl,
                findall(Res , defending_list(C, List), List2),
                print(List2),nl.
                % check_admissible(C, List2).

defending_list(L, Res):- print(L), nl, print(Res), nl, defending_list(L, Res, []).
defending_list([], Acc, Acc):- print("Acc"), print(Acc),nl.

defending_list([H|T], [[H,X]|Res], Acc):-
    attacks(List),
    member([H,X], List),
    % member(D, [H|T]),
    % member(A, Res),
    % member([D, A], List),
    print(H),nl,
    print(Res),nl,
    % print(H),nl,
    % print(T),nl,
    % member([H, X], List),
    % append(Acc, [X], NewAcc),
    defending_list(T, Res, Acc).

defending_list([H|T], [[H2,X]|Res], Acc):-
    attacks(List),

    \+(member([H2,X], List)),
    defending_list(T, Res, Acc).


% defending_list([H|T], [[H2,X]|Res], Acc):-
%     H \= H2,
%     append(Acc, X, NewAcc),
%     defending_list(T, Res, NewAcc).


attacking_list(L, Res):- attacking_list(L, Res, []).
attacking_list([], Acc, Acc).

attacking_list([H|T], Res, Acc):-
    attacks(List),
    member([X, H], List),
    % print(X),nl,
    % print(Acc),nl,
    append(Acc, [X], NewAcc),
    % print(NewAcc),nl,
    attacking_list(T, Res, NewAcc).

check_admissible(_, []).
check_admissible(C, [[Attacker, Defender]|T]):-
  attacks(List),
  \+(member([Attacker, Defender], List)),
  member(Attacker, C),
  member(Defender, C),
  check_admissible(C, T).


% Edge cases
% Singleton_argument
% Should conflict_free(X) return a set of conflict-free??
% conflict_free([c,f]) when f doesn't exist
% conflict_free([a,a]) -> conflict_free(a)
% perm([1,2,3,1], X).
