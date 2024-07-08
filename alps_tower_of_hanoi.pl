% ======= Helper Methods =======

% Joins lists `A` and `B`, producing `C`
% `join(A, B, C).
join([], L, L).
join([H|T], L, [H|R]) :- join(T, L, R).

% ======= =======


% ======= Tower of Hanoi Puzzle =======
% Find the steps to solve the Tower of Hanoi puzzle with a tower of height `N`.

% Base case - a singular piece to move from source to target:
tower_of_hanoi_h(1,SRC,TAR,_, [[SRC|TAR]]) :-  
   write('Top piece of `'), write(SRC), write('` -> `'), write(TAR), write('`'), nl.

% Recursive case:
tower_of_hanoi_h(N,SRC,TAR,INT, R) :-
   N > 1, M is N - 1,
   tower_of_hanoi_h(M,SRC,INT,TAR, R1),  % Create a tower in the intermediate of all smaller pieces.
   tower_of_hanoi_h(1,SRC,TAR,_, R2),    % Move the remaining (largest) piece to the target.
   join(R1, R2, R3),
   tower_of_hanoi_h(M,INT,TAR,SRC, R4),  % Transfer the tower in the intermediate to the target.
   join(R3, R4, R).

% Entry point method:  
% Write the steps for solving Tower of Hanoi of height `N` and produce a list of them as `R`.
% `tower_of_hanoi_h(1,SOURCE,TARGET,INTERMEDIATE,R).`
% `R` is a list of moves in the format: [... [FROM|TO], ...]
tower_of_hanoi(N, R) :- N > 0,tower_of_hanoi_h(N, src, tar, int, R).

% ======= =======

factorial(0, 1).
factorial(N, R) :- N > 0, N1 is N - 1, factorial(N1, R1), R is R1 * N.

fibbonaci(0, 1).
fibbonaci(1, 1).
fibbonaci(N, R) :- 
   N > 1, 
   N1 is N - 1, 
   fibbonaci(N1, R1), 
   N2 is N - 2, 
   fibbonaci(N2, R2), 
   R is R1 + R2.



