% ======= Helper Methods =======

% Returns if `X` is in a list `Y`:
%`in(X, Y)`
in(X, [X|_]).
in(X, [_|T]) :- in(X, T).

% Joins lists `A` and `B`, producing `C`
% `join(A, B, C).
join([], L, L).
join([H|T], L, [H|R]) :- join(T, L, R).

% Find the length of `A`, producing number `R`
% `len(A, R)`
len([], 0).
len([_|T], R) :- len(T, Y), R is Y + 1.

% ======= =======


% ======= Set Operations =======

% Convert list `A` into a set `B`
% `set(A, B)`
set([], []).  
set([H|T], B) :- set(T, B), in(H, B).  % If `H` is already in set: don't add
set([H|T], [H|B]) :- set(T, B).  % Else: add and continue


% Produces `C`: union of sets `A` and `B`
union([], B, B).  % Base case
union([H|T], B, C) :- (in(H, B) -> union(T, B, C); 
        union(T, B, C1), C=[H|C1]).  % Element already in `A` & `B`: skip


% !!! Bug with not stopping and returning if the user denies the given result; continues to print other non-answers

% Produces `C`: intersection of sets `A` and `B`
% `intersection(A, B, C)`
intersection([], _, []).  % Base case
intersection([H|T], B, [H|C]) :- in(H, B), intersection(T, B, C).  % Element in `A` & `B`: add to result
intersection([_|T], B, C) :- intersection(T, B, C).  % Else: skip element


% Produces `C`: difference of sets `A` and `B`
% `difference(A, B, C)`  *`_h` designates helper method, not to be interacted w/ by user
difference([], _, []).
difference([H|T], B, C) :- in(H, B), difference(T, B, C).  % Element in `A` & B`: subtract from result
difference([H|T], B, [H|C]) :- difference(T, B, C).  % Else: save element and continue

% ======= =======


% ======= Challege: power_set =======

% Insert `X` at the start of all lists in `A`, producing `R`
% `insert_start_all(A, X, R)`
insert_start_all([], _, []).
insert_start_all([H|T], X, [[X|H]|R]) :- insert_start_all(T, X, R).


% Produce a list `R` where all elements `I` in `A` are wrapped in a list i.e. `[I]`
% `wrap_all(A, R)`
wrap_all([], []).
wrap_all([H|T], [[H]|R]) :- wrap_all(T, R).


% Generate list of combinations of len `N` or less from `A`, producing `R`
% `comb(N, A, R)`
comb(0, _, [[]]).
comb(1, A, [[]|R]) :- wrap_all(A, R).
comb(N, [H|T], R) :- K is N - 1, comb(K, T, P), insert_start_all(P, H, Q), join(P, Q, R).


% Makes a list `B` of the combinations of elements in set `A`
% `power_set(A, B)` *`_h` designates helper method, not to be interacted w/ by user
power_set(A, B) :- len(A, N), comb(N, A, B).

% ======= =======
