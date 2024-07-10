% ======= Wordle Application =======

% Process:
% Not-written-yet ;P

% === Util Methods ===

% Joins lists `A` and `B`, producing `C`
% `join(A, B, C).
join([], L, L).
join([H|T], L, [H|R]) :- join(T, L, R).


% Returns if `X` is in a list `Y`:
%`in(X, Y)`
in(X, [X|_]).
in(X, [_|T]) :- in(X, T).


% Returns if `X` is not in a list `Y`:
%`not_in(X, Y)`
not_in(_, []).
not_in(X, [H|T]) :- X \== H, not_in(X, T).


% Deletes the first occurrence (if there is one) of an element `X` from list `Y`, producing `R`.
% `del(X, Y, R).`
del(_, [], []).
del(X, [X|Ys], Ys).
del(X, [Y|Ys], [Y|Zs]) :- del(X, Ys, Zs).


% Adds all elements in `B` to the front of all lists in `A`, producing `C`
% `add_all(A, B, C).`
add_all_to_all(X, [], X).
add_all_to_all(H, [H2|T2], X) :- 
	add_to_all(H, H2, X1), add_all_to_all(X1, T2, X).

add_to_all([], _, []).
add_to_all([H|T], X, [[X|H]|P]) :- add_to_all(T, X, P).

% Convert list `A` into a set `B`
% `set(A, B)`
set([], []).
set([H|T], B) :- set(T, B), in(H, B).  % If `H` is already in set: don't add
set([H|T], [H|B]) :- set(T, B).  % Else: add and continue


% === ===

letters([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).

% Read the word list in `./words.txt` into a list `R`.
% `words(R).`
words(R) :-
	open('./words.txt', read, Str),
  read_file(Str, R_W_ENDOFFILE),
	del([e,n,d,_,o,f,_,f,i,l,e], R_W_ENDOFFILE, R),!,
  close(Str).
read_file(Stream,[]) :-
  at_end_of_stream(Stream).
read_file(Stream,[WORD|L]) :-
  \+ at_end_of_stream(Stream),
  read(Stream, X),
	atom_chars(X, WORD),
  read_file(Stream, L).

	
% Produces the `FEEDBACK` list from the given `GUESS` on the target word `TAR`
% Info key: 0=not-in, 1=wrong-pos, 2=correct
% `gen_feedback(TAR, GUESS, FEEDBACK).`
gen_feedback(T, G, F) :- 
	gen_feedback_h_exacts(T, G, E, L),
	gen_feedback_h_others(T, G, O, L),
	gen_feedback_h_merge_e_o(E, O, F).

% `gen_feedback_h_exacts(TAR, GUESS, FEEDBACK, REMAINING_LETTERS).`
gen_feedback_h_exacts([], [], [], []).
gen_feedback_h_exacts([T_H|T_T], [G_H|G_T], [F1|F2], L) :-
	(T_H == G_H ->
		F1 = 2, L = L2;
		F1 = 0,	L = [T_H|L2]
	),
	gen_feedback_h_exacts(T_T, G_T, F2, L2).

% `gen_feedback_h_others(TAR, GUESS, FEEDBACK, REMAINING_LETTERS).`
gen_feedback_h_others([], [], [], _).
gen_feedback_h_others([T_H|T_T], [G_H|G_T], [F1|F2], L) :-
	T_H == G_H, F1 = 0, gen_feedback_h_others(T_T, G_T, F2, L).
gen_feedback_h_others([T_H|T_T], [G_H|G_T], [F1|F2], L) :-
	T_H \== G_H,
	(in(G_H, L) -> 
		del(G_H, L, L1),!,
		F1 = 1,
		gen_feedback_h_others(T_T, G_T, F2, L1);
		F1 = 0,
		gen_feedback_h_others(T_T, G_T, F2, L)
	).

gen_feedback_h_merge_e_o([], [], []).
gen_feedback_h_merge_e_o([E_H|E_T], [O_H|O_T], [F1|F2]) :-
	(E_H == 2 -> F1 = 2;
	(O_H == 1 -> F1 = 1; F1 = 0)),
	gen_feedback_h_merge_e_o(E_T, O_T, F2).


% TODO: CONSTRAINTS TO ELIMINATE MORE CHARS PER SPOTS

% Produces the `A` constraint from a `GUESS` and its `FEEDBACK`
% 
gen_A_constraints([], [], []).
gen_A_constraints([G_H|G_T], [F_H|F_T], [A|L]) :-
	(F_H == 2 -> A = G_H; A = 0),
	gen_A_constraints(G_T, F_T, L).


% Produces the `B` constraint from a `GUESS` and its `FEEDBACK`
% 
gen_B_constraints([], [], []).
gen_B_constraints([G_H|G_T], [F_H|F_T], B) :- 
	(F_H \== 0 -> B = [G_H|L]; B = L),
	gen_B_constraints(G_T, F_T, L).


% Produces the `C` constraint from a `GUESS` and its `FEEDBACK`
% 
gen_C_constraints(G, F, C) :- 
	singular_non_contained(G, F, S),
	global_non_contained(G, F, L),
	add_all_to_all(S, L, C).
singular_non_contained([], [], []).
singular_non_contained([G_H|G_T], [F_H|F_T], S) :-
	(F_H == 1 -> S = [[G_H]|L]; S = [[]|L]),
	singular_non_contained(G_T, F_T, L).
global_non_contained([], [], []).
global_non_contained([G_H|G_T], [F_H|F_T], G) :-
	(F_H == 0 -> G = [G_H|L]; G = L),
	global_non_contained(G_T, F_T, L).


% Produces the constraint sets `A`, `B`, and `C` for a given `GUESS`  and `FEEDBACK`
% Constraints key: A=CORRECT, B=CONTAINED, C=NOT_CONTAINED
% `gen_constraints(GUESS, FEEDBACK, A, B, C).`
% Constraint Explanation: Not written yet 
gen_constraints([], [], [], [], []).
gen_constraints(G, F, A, B, C) :-
	gen_A_constraints(G, F, A),  % Exacts that are known
	gen_B_constraints(G, F, B),  % All letters which are in word
	gen_C_constraints(G, F, C).  % For each pos, the letters it CAN'T be


% Produce a `CANDS` list of possible words from OLD_CANDS which match new constraints
% TODO: `gen_new_cands(...).`
gen_cands(OLD_CANDS, A, B, C, NEW_CANDS) :-
	findall(W, matching_word(OLD_CANDS, A, B, C, W), NEW_CANDS).


% Produces a word from the word set `W` which fits constraints `A`, `B`, and `C`
% `matching_word(W, A, B, C, WORD).`
matching_word(W, [A1, A2, A3, A4, A5], B0, [C1,C2,C3,C4,C5], R) :- 
% Change w/ gen_cands to iterate through words and check if each satisfies; if so, add 
% to next CANDS list (instead of using `findall` and counting dupes)
	% In word set
	in([A, B, C, D, E], W),

	% All in contained-chars set is contained
	del(A, B0, B1),
	del(B, B1, B2),
	del(C, B2, B3),
	del(D, B3, B4),
	del(E, B4, []),

	% Set all correct variables
  (A1 \== 0 -> A = A1; A = A),
	(A2 \== 0 -> B = A2; B = B),
	(A3 \== 0 -> C = A3; C = C),
	(A4 \== 0 -> D = A4; D = D),
	(A5 \== 0 -> E = A5; E = E),
	
	% Each is not in non-contained set or is preset
	(not_in(A, C1); A == A1),
	(not_in(B, C2); B == A2),
	(not_in(C, C3); C == A3),
	(not_in(D, C4); D == A4),
	(not_in(E, C5); E == A5),

	R = [A,B,C,D,E].


% Produces a `GUESS` from the `CANDS` list
% `gen_guess(CANDS, GUESS)`
gen_guess([], [0,0,0,0,0]).  % Temp
gen_guess([H|_], H).  % Temp 


% Entrance Method to start wordle solver
% Note: A stream does not exist error usually means this method was back-tracked to
wordle :-
	words(W),
	write('Enter solver type: (type "auto." or "manual."):'), nl,
	read(TYPE),
	(TYPE == auto ->
		write('"auto" mode selected; enter target word:'), nl,
		read(TAR_ATOM),
		atom_chars(TAR_ATOM, TAR),
		auto_wordle(W, TAR, G),
		write(G)
		;
		manual_wordle(W, G),
		write(G)
	).
	

% Auto-wordle solver: player chooses a word `TAR`, and the program
% outputs `GUESSES` used to try and guess the word.
% Program automatically checks guess info, instead of having user input (see manual_wordle)
auto_wordle(W, TAR, GUESSES) :-
	CANDS1 = W,

	% Guess 1
	gen_guess(CANDS1, G1),
	gen_feedback(TAR, G1, I1),
	gen_constraints(G1, I1, A1, B1, C1),
	gen_cands(CANDS1, A1, B1, C1, CANDS2),

	% Guess 2
	gen_guess(CANDS2, G2),
	gen_feedback(TAR, G2, I2),
	gen_constraints(G2, I2, A2, B2, C2),
	gen_cands(CANDS2, A2, B2, C2, CANDS3),
	
	% Guess 3
	gen_guess(CANDS3, G3),
	gen_feedback(TAR, G3, I3),
	gen_constraints(G3, I3, A3, B3, C3),
	gen_cands(CANDS3, A3, B3, C3, CANDS4),
	
	% Guess 4
	gen_guess(CANDS4, G4),
	gen_feedback(TAR, G4, I4),
	gen_constraints(G4, I4, A4, B4, C4),
	gen_cands(CANDS4, A4, B4, C4, CANDS5),

	% Guess 5
	gen_guess(CANDS5, G5),
	gen_feedback(TAR, G5, I5),
	gen_constraints(G5, I5, A5, B5, C5),
	gen_cands(CANDS5, A5, B5, C5, CANDS6),

	% Guess 6
	gen_guess(CANDS6, G6),
	
	% Decide what to return  % TODO: Clean up this 
	(G1 == TAR -> 
		GUESSES = [G1];
		(G2 == TAR -> 
			GUESSES = [G1, G2];
			(G3 == TAR -> 
				GUESSES = [G1, G2, G3];
				(G4 == TAR -> 
					GUESSES = [G1, G2, G3, G4];
					(G5 == TAR -> 
						GUESSES = [G1, G2, G3, G4, G5];
						(G6 == TAR -> 
							GUESSES = [G1, G2, G3, G4, G5, G6];
							GUESSES = [G1, G2, G3, G4, G5, G6, not_found]
						)
					)
				)
			)
		)
	).


% Manual-wordle solver: player chooses a word `TAR`, and the program
% outputs `GUESSES` used to try and guess the word.
% Program awaits user input to validate guess info, instead of doing it itself (see auto_wordle)
manual_wordle(W, GUESSES) :-  
% ALERT: DOESN'T WORK DUE TO NOT BEING UPDATED YET TO REFLECT NEW METHOD STRUCTURES
	CANDS1 = W,

	% Guess 1
	gen_guess([[c,r,a,n,e]], G1),
	write(G1), nl,
	read(I1),
	gen_constraints(G1, I1, A1, B1, C1),
	findall(WORD, matching_word(WORD, CANDS1, A1, B1, C1), CANDS2),

	% Guess 2
	gen_guess(CANDS2, G2),
	write(G2), nl,
	read(I2),
	gen_constraints(G2, I2, A2, B2, C2),
	findall(WORD, matching_word(WORD, CANDS2, A2, B2, C2), CANDS3), 

	% Guess 3
	gen_guess(CANDS3, G3),
	write(G3), nl,
	read(I3),
	gen_constraints(G3, I3, A3, B3, C3),
	findall(WORD, matching_word(WORD, CANDS3, A3, B3, C3), CANDS4),
	
	% Guess 4
	gen_guess(CANDS4, G4),
	write(G4), nl,
	read(I4),
	gen_constraints(G4, I4, A4, B4, C4),
	findall(WORD, matching_word(WORD, CANDS4, A4, B4, C4), CANDS5),

	% Guess 5
	gen_guess(CANDS5, G5),
	write(G5), nl,
	read(I5),
	gen_constraints(G5, I5, A5, B5, C5),
	findall(WORD, matching_word(WORD, CANDS5, A5, B5, C5), CANDS6),

	% Guess 6
	gen_guess(CANDS6, G6),
	write(G6), nl,
	read(I6),
	gen_constraints(G6, I6, A6, B6, C6),
	findall(WORD, matching_word(WORD, CANDS6, A6, B6, C6), _),

	GUESSES = [G1, G2, G3, G4, G5, G6].


