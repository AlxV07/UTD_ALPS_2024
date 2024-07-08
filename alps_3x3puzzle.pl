% Deletes the first occurance of an element `X` from list `Y`, producing `R`.
% `del(X, Y, R).`
del(X, [X|Ys], Ys).
del(X, [Y|Ys], [Y|Zs]) :- del(X, Ys, Zs).


% Solves the 3x3 Puzzle with a given bucket of digits `D` to make sums `S`.
solve3x3puzzle([A11, A12, A13, A21, A22, A23, A31, A32, A33], D, S) :-
   %  === Dels ===
   del(A11, D, D1),
   del(A21, D1, D2),
   del(A31, D2, D3),
   
   del(A12, D3, D4),
   del(A22, D4, D5),
   del(A32, D5, D6),

   del(A13, D6, D7),
   del(A23, D7, D8),
   del(A33, D8, _),
   
   % === Cols Constraints ===
   A11 + A21 + A31 =:= S,
   A12 + A22 + A32 =:= S,
   A13 + A23 + A33 =:= S,
   
   % === Rows Constraints ===
   A11 + A12 + A13 =:= S,
   A21 + A22 + A23 =:= S,
   A31 + A32 + A33 =:= S,

   % === Diaganols Constraints ===
   A11 + A22 + A33 =:= S,
   A13 + A22 + A31 =:= S.
   