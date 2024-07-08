% Insert an element `X` into a list `L`, producing `R`.
% `insert(X, L, R).`
insert(X, [], [X]).
insert(X, [H], R) :- 
    (X =< H -> 
        R = [X, H]; 
        R = [H, X]).
insert(X, [H|T], R) :- 
    T \== [], 
    (X =< H -> 
        R = [X|[H|T]]; 
        insert(X, T, R1), R = [H|R1]).


% Insert sort a list `L` producing `R`.
% `insert_sort(L, R).`
insert_sort([], []).
insert_sort([H|T], R) :- 
    insert_sort(T, R1), insert(H, R1, R).
