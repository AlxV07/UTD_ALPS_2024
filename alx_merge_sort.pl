% ======= Other =======

% Find the smallest number `R` less than `H` in a list `A`.
% `min(H, A, R).` 
min_h(H, [], H).
min_h(H, [X|T], M) :- min_h(X, T, M1), H > M1 -> M = M1; M = H. 
min([X|T], M) :- min_h(X, [X|T], M).

% Find the length of `A`, producing number `R`
% `len(A, R)`
len([], 0).
len([_|T], R) :- len(T, Y), R is Y + 1.

% ======= =======


% ======= Merge Sort =======

% ===
% Returns whether a list `L` has a length longer than `N`
% `longer_than(L, N).`
longer_than([_|_], 0).
longer_than([_|T], N) :- N > 0, N1 is N - 1, longer_than(T, N1).
% ===

% ===
% Split a list `L` of length `P` into halves `R1` and `R2`.
% `split_in_half(L, P, R1, R2).`
split_in_half(L, P, R1, R2) :-
    MID is P // 2,
    part_before(L, MID, R1),
    part_after(L, MID, R2).

% Sublist of `L` before `N` (non-inclusive), produces `R`
% `part_before(L, N, R).`
part_before(_, 0, []).
part_before([H|T], N, [H|R]) :-
    N > 0, N1 is N - 1,
    part_before(T, N1, R).

% Sublist of `L` after `N` (inclusive), produces `R`
% `part_after(L, N, R).`
part_after(L, 0, L).
part_after([_|T], N, R) :-
    N > 0, N1 is N - 1,
    part_after(T, N1, R).
% ===

% ===
% Joins sorted lists `H1` and `H2` producing an ordered list `R`.
% `combine_sorted(H1, H2, R).`
combine_sorted([], [], []).
combine_sorted([], A, A).
combine_sorted(A, [], A).
combine_sorted([H1|T1], [H2|T2], [H1|R1]) :-
    H1 =< H2, 
    combine_sorted(T1, [H2|T2], R1).
combine_sorted([H1|T1], [H2|T2], [H2|R1]) :-
    H1 > H2,
    combine_sorted([H1|T1], T2, R1).
% ===

% ===
% Merge sort a list `L` of length `P`, and produce `R`.
% `merge_sort_h(L, P, R).`
merge_sort_h([], 0, []).
merge_sort_h([A], 1, [A]).
merge_sort_h([A,B], 2, R) :- A =< B -> R = [A,B]; R = [B,A].
merge_sort_h(L, P, R) :-
   P > 2, P1 is P // 2, P2 is P - P1,
   split_in_half(L, P, H1, H2),
   merge_sort_h(H1, P1, R1),
   merge_sort_h(H2, P2, R2),
   combine_sorted(R1, R2, R).

% Entrance point: Merge sort a list `L`, producing `R.
% merge_sort(L, R).
merge_sort(L, R) :- len(L, P), merge_sort_h(L, P, R).
% ===

% ======= =======