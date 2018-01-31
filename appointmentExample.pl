myAsm(free6(a)).
myAsm(free8(a)).
myAsm(free6(b)).
myAsm(free8(b)).
myAsm(sports(b)).
myAsm(child(a)).
myAsm(overweight(b)).

myAsm(not_get6(a)).
myAsm(not_get8(a)).
myAsm(not_get6(b)).
myAsm(not_get8(b)).

myRule(get6(a), [free6(a),not_get6(b),not_get8(a)]).
myRule(get6(b), [free6(b),not_get6(a),not_get8(b)]).
myRule(get8(b), [free8(b),not_get8(a),not_get6(b)]).
myRule(get8(a), [free8(a),not_get8(b),not_get6(a)]).

myRule(not_free6(b), [sports(b)]).
myRule(not_free8(a), [child(a)]).
myRule(not_sports(b), [overweight(b)]).

contrary(free6(a), not_free6(a)).
contrary(free6(b), not_free6(b)).
contrary(free8(a), not_free8(a)).
contrary(free8(b), not_free8(b)).

contrary(not_get6(a), get6(a)).
contrary(not_get6(b), get6(b)).
contrary(not_get8(a), get8(a)).
contrary(not_get8(b), get8(b)).

contrary(sports(b), not_sports(b)).
contrary(child(a), not_child(a)).
contrary(overweight(b), not_overweight(b)).
