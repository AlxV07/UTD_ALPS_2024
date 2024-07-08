% ======= Helper Methods =======

% Produces a list of numbers when `N > 0`, `[N, N-1, N-2, ... 1]` as `R`.
% `range(N, R).`
range(0, []).
range(N, [N|R]) :- K is N - 1, range(K, R).

% Deletes the first occurance of an element `X` from list `Y`, producing `R`.
% `del(X, Y, R).`
del(X, [X|Ys], Ys).
del(X, [Y|Ys], [Y|Zs]) :- del(X, Ys, Zs).

% ======= =======


% ======= N-Queens Puzzle =======

% Given `N` number of Queens, produces a candidate list `R` of positions on an `N x N` size board.
% `R` is in the format: each `i`-th element represents a queen in the `i`-th col in the `R[i]` row.

% Entrance point:
queens(N, R):- range(N, Ns), queens_h(Ns, [], R).

% Base case:  No more queens to place, return what we've got.
queens_h([], Qs, Qs).  

% Recursive case:
queens_h(Unplaced, Placed, Qs) :-
    del(Q, Unplaced, NewUnplaced),  % Remove the current queen from the `Unplaced` list (it's placed now)
    no_attack(Q, Placed),           % Place queen on col without being attacked.
    queens_h(NewUnplaced, [Q|Placed], Qs).  % Recursive call on remaining queens.
  

% Entrance point:
no_attack(Q, Safe) :- no_attack_h(Safe, Q, 1).

% Base case: No queens attacked, is safe.
no_attack_h([], _, _).

% Recursive case: 
no_attack_h([Y|Ys], Q, Nb) :-
    % Not attacked
    Q =\= Y + Nb,  
    Q =\= Y - Nb,  
    Nb1 is Nb + 1,
    no_attack_h(Ys, Q, Nb1).  % Since Prolog is cool it will backtrack if there is a conflicting queen.

% ======= =======



%  ======= Lucas Lin's code: =======
safe([]).
safe([Q|Qs]) :- safe(Qs), \+ attack(Q, Qs).
attack(X, Xs) :- attack(X, 1, Xs).
attack(X, N, [Y|Ys]) :- X is Y+N; X is Y-N.
attack(X, N, [Y|Ys]) :- N1 is N+1, attack(X, N1, Ys).
queens([], Qs, Qs).
queens_b(N, Qs) :- range(N, Ns), queens(Ns, [], Qs).  % Entrance point method
queens(UnplacedQs, SafeQs, Qs) :- 
    del(Q, UnplacedQs, Qs1),
    \+ attack(Q, SafeQs),
queens(Qs1, [Q|SafeQs], Qs).

% ======= =======
