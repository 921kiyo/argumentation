X = (free6(a),{free6(a)}) ? ;
X = (free6(b),{free6(b)}) ? ;
X = (child(a),{child(a)}) ? ;
X = (overweight(b),{overweight(b)}) ? ;
X = (not_get8(a),{not_get8(a)}) ? ;
X = (not_get6(b),{not_get6(b)}) ? ;
X = (not_get8(b),{not_get8(b)}) ? ;
X = (get6(a),{free6(a),not_get6(b),not_get8(a)}) ? ;
X = (not_free8(a),{child(a)}) ? ;
X = (not_sports(b),{overweight(b)}) ? ;


X = (free6(a),{free6(a)}) ? ;
X = (free6(b),{free6(b)}) ? ;
X = (child(a),{child(a)}) ? ;
X = (overweight(b),{overweight(b)}) ? ;
X = (not_get6(a),{not_get6(a)}) ? ;
X = (not_get8(a),{not_get8(a)}) ? ;
X = (not_get6(b),{not_get6(b)}) ? ;
X = (get6(b),{free6(b),not_get6(a),not_get8(b)}) ? ;
X = (not_free8(a),{child(a)}) ? ;
X = (not_sports(b),{overweight(b)}) ? ;


% 1. X = (free6(a),[free6(a)]) ? ;
2. X = (free8(a),[free8(a)]) ? ;
3. X = (free6(b),[free6(b)]) ? ;
4. X = (free8(b),[free8(b)]) ? ;
% 5. X = (sports(b),[sports(b)]) ? ;
% 6. X = (child(a),[child(a)]) ? ;
7. X = (overweight(b),[overweight(b)]) ? ;
8. X = (not_get6(a),[not_get6(a)]) ? ;
9. X = (not_get8(a),[not_get8(a)]) ? ;
% 10. X = (not_get6(b),[not_get6(b)]) ? ;
% 11. X = (not_get8(b),[not_get8(b)]) ? ;
% 12. X = (get6(a),[free6(a),not_get6(b),not_get8(a)]) ? ;
13. X = (get6(b),[free6(b),not_get6(a),not_get8(b)]) ? ;
14. X = (get8(b),[free8(b),not_get8(a),not_get6(b)]) ? ;
15. X = (get8(a),[free8(a),not_get8(b),not_get6(a)]) ? ;
% 16. X = (not_free6(b),[sports(b)]) ? ;
% 17. X = (not_free8(a),[child(a)]) ? ;
18. X = (not_sports(b),[overweight(b)]) ? ;
