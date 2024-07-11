% ======= Wordle Application =======

% This solver's process for determining a word:
% Start with a massive word set.
% Arbitrarly pick a word as a guess from the candidate set.
% Determine "feedback" from the guess:
%  - which letters in the guess are correct
%  - which letters in the guess are in the wrong position
%  - which letters in the guess are not included
% From this feedback, build constraints to apply to the candidate set:
%  - "A" constraint: list of known correct letters in their respective positions, 0 if letter pos is not known.
%    E.x. [c,0,0,n,e]
%  - "B" constraint: a list of all the known letters in the word which must be used in the guess.
%    E.x. [c,n,e,r]
%  - "C" constraint: a list of lists of letters which are known not to be at a pos.
%    E.x. [[p], [p], [o, p], [d, p], [p]]
% Using these constraints, eliminate candidates from candidate set which do not satisfy.
% Repeat until out of guesses, candidate set is expired, or solution is found.
		
% === HELP ===
% This wordle solver has two modes: "auto.", and "manual."
% *Note the periods at the end of text in quotes. When entering input into this solver, a period must always end the query.
% = "auto." mode: =
% The user will enter a word into the solver and the solver will print out on a new line each guess which was used in attempt to determine the word (if not determined, it will also print so after the guesses).
% = "manual." mode: 
% The user will pick a word and the solver will try to guess it in six guesses.  Each guess, the user will give "feedback" on the guessed word: what letters are correct, what are in the wrong positions, what are not included, etc. Choose the "manual." mode for more details on entering feedback.
% === END OF HELP ===


% === Util Methods ===

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


% Adds `B` to the front of all lists in `A`, producing `C`.
% `add_to_all(A, B, C).`
add_to_all([], _, []).
add_to_all([H|T], X, [[X|H]|P]) :- add_to_all(T, X, P).


% Concats elements of a list `L` into an atom `R`
% `concat_list(L, R).`
concat_list([A], A).
concat_list([A|B], R) :- concat_list(B, R1), atom_concat(A, R1, R).


% Prints all elements in a list `L` on a new line, enumerated e.x. ith-element `E` -> `i: E`.
% `enum_print_all(L)`.
enum_print_all(L) :- enum_print_all_h(L, 1).
enum_print_all_h([], _).
enum_print_all_h([H|T], I) :- write(I), write(': '), write(H), nl, I1 is I + 1, enum_print_all_h(T, I1).


% Produces a list `R` of dupes in `W`.
% [f,a,n,t,a] -> [a]
% [a,h,a,h,a] -> [a,a]
% `dupes(W, R).`
dupes(W, R) :- dupes_h(W, W, R).
dupes_h(_, [], []).
dupes_h(W, [H|T], R) :-
	del(H, W, W1),!,
	(in(H, W1) -> R = [H|R1]; R = R1),
	dupes_h(W1, T, R1).

% === ===


% === Word Set Gen ===

% Read the word list in `./words.txt` into a list `R`.
% Does not backtrack.
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

% === ===
	

% === Feedback Gen === 

% Produces the `FEEDBACK` list from the given `GUESS` on the target word `TAR`
% Info key: n=not-in, i=in,wrong-pos, c=correct
% `gen_feedback(TAR, GUESS, FEEDBACK).`
gen_feedback(T, G, F) :- 
	gen_feedback_h_exacts(T, G, E, L),
	gen_feedback_h_others(T, G, O, L),
	gen_feedback_h_merge_e_o(E, O, F).

% Produces a partial `FEEDBACK` list containing only `c` desginators.
% `gen_feedback_h_exacts(TAR, GUESS, FEEDBACK, REMAINING_LETTERS).`
gen_feedback_h_exacts([], [], [], []).
gen_feedback_h_exacts([T_H|T_T], [G_H|G_T], [F1|F2], L) :-
	(T_H == G_H ->
		F1 = c, L = L2;
		F1 = n,	L = [T_H|L2]
	),
	gen_feedback_h_exacts(T_T, G_T, F2, L2).

% Produces a partial `FEEDBACK` list containing only `i` desginators.
% `gen_feedback_h_others(TAR, GUESS, FEEDBACK, REMAINING_LETTERS).`
gen_feedback_h_others([], [], [], _).
gen_feedback_h_others([T_H|T_T], [G_H|G_T], [F1|F2], L) :-
	T_H == G_H, F1 = 0, gen_feedback_h_others(T_T, G_T, F2, L).
gen_feedback_h_others([T_H|T_T], [G_H|G_T], [F1|F2], L) :-
	T_H \== G_H,
	(in(G_H, L) -> 
		del(G_H, L, L1),!,
		F1 = i,
		gen_feedback_h_others(T_T, G_T, F2, L1);
		F1 = n,
		gen_feedback_h_others(T_T, G_T, F2, L)
	).

% Merges `EXACTS`, and `OTHERS`, partial feedback lists into the final `FEEDBACK`
% `gen_feedback_h_merge_e_o(EXACTS, OTHERS, FEEDBACK).`
gen_feedback_h_merge_e_o([], [], []).
gen_feedback_h_merge_e_o([E_H|E_T], [O_H|O_T], [F1|F2]) :-
	(E_H == c -> F1 = c;
	(O_H == i -> F1 = i; 
	F1 = n)),
	gen_feedback_h_merge_e_o(E_T, O_T, F2).

% === ===


% === Constraint Gen ===

% Produces the constraint sets `A`, `B`, and `C` for a given `GUESS`  and `FEEDBACK`
% Constraints key: A=CORRECT, B=CONTAINED, C=NOT_CONTAINED
% `gen_constraints(GUESS, FEEDBACK, A, B, C).`
gen_constraints([], [], [], [], []).
gen_constraints(G, F, A, B, C) :-
	gen_A_constraints(G, F, A),  % Exacts that are known
	gen_B_constraints(G, F, B),  % All letters which are in word
	gen_C_constraints(G, F, C).  % For each pos, the letters it CAN'T be

% Produces the `A` constraint from a `GUESS` and its `FEEDBACK`
% `gen_A_constraints(GUESS, FEEDBACK, A)`.
gen_A_constraints([], [], []).
gen_A_constraints([G_H|G_T], [F_H|F_T], [A|L]) :-
	(F_H == c -> A = G_H; A = 0),
	gen_A_constraints(G_T, F_T, L).

% Produces the `B` constraint from a `GUESS` and its `FEEDBACK`
% `gen_B_constraints(GUESS, FEEDBACK, B)`.
gen_B_constraints([], [], []).
gen_B_constraints([G_H|G_T], [F_H|F_T], B) :- 
	(F_H \== n -> B = [G_H|L]; B = L),
	gen_B_constraints(G_T, F_T, L).

% Produces the `C` constraint from a `GUESS` and its `FEEDBACK`
% `gen_C_constraints(GUESS, FEEDBACK, C)`.
gen_C_constraints(G, F, C) :- 
	singular_non_contained(G, F, S),
	dupes(G, D),
	global_non_contained(G, F, L, D),
	add_all_to_all(S, L, C).

% Produces a list `L`, of singularly non-contained letters i.e. not in the cur pos in the word.
% `singular_non_contained(GUESS, FEEDBACK, L).`
singular_non_contained([], [], []).
singular_non_contained([G_H|G_T], [F_H|F_T], S) :-
	(F_H == i -> S = [[G_H]|L]; S = [[]|L]),
	singular_non_contained(G_T, F_T, L).

% Produces a list `L`, of globally non-contained letters i.e. not in any pos in the word.
% `global_non_contained(GUESS, FEEDBACK, L, DUPES).`
global_non_contained([], [], [], _).
global_non_contained([G_H|G_T], [F_H|F_T], G, D) :-
	(F_H == n ->
		del(G_H, D, D1), (D == D1 -> G = [G_H|L]; G = L);
		G = L, D1 = D),
	global_non_contained(G_T, F_T, L, D1).  

% === ===


% === Guess Gen ===

% Produce a `NEW_CANDS` list of possible words from OLD_CANDS which match new constraints
% `gen_new_cands(OLD_CANDS, A, B, C, NEW_CANDS).`
gen_cands([], _, _, _, []).
gen_cands([O_H|O_T], A, B, C, NEW_CANDS) :-
	(matching_word(O_H, A, B, C) ->
		NEW_CANDS = [O_H|N2];
		NEW_CANDS = N2
	),
	gen_cands(O_T, A, B, C, N2).


% Returns if a word W matches the constraints `A`, `B`, and `C`.
% `matching_word(W, A, B, C).`
matching_word([A,B,C,D,E], [A1, A2, A3, A4, A5], B0, [C1,C2,C3,C4,C5]) :- 
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
	(not_in(E, C5); E == A5).


% Produces a `GUESS` from the `CANDS` list
% `gen_guess(CANDS, GUESS)`
gen_guess([], [0,0,0,0,0]).  % If reaches this, either unfound bug or TAR word not in word set.
gen_guess(L, G) :-
	L \== [],
	length(L, LEN), L1 is LEN - 1,
	random_between(0, L1, B),
	nth0(B, L, G).

% === ===


% === Wordle Methods ===

% Entrance Method to start wordle solver
wordle :-
	words(W),
	write('Enter solver mode: ("auto." or "manual.", type "help." for help):'), nl,
	read(MODE),
	(MODE == auto ->
		write('=== "auto." mode selected ==='), nl,
		write('Enter target word e.x. "crane."'), nl,
		read(TAR_ATOM),	atom_chars(TAR_ATOM, TAR),
		auto_wordle(W, TAR, G),
		enum_print_all(G);
	(MODE == manual ->
		write('=== "manual." mode selected ==='), nl,
		write('For each guess from the solver, enter feedback in the format:'), nl,
		write('-Letter is correct? ->                       c'), nl,
		write('-Letter in word and NOT already marked*? ->  i'), nl,
		write('-Letter is not in word? ->                   n'), nl,
		write('* i.e. Word="ab", Guess="aa" -> '), nl,
		write('*      Feedback="cn", not "ci", because first "a" already marked by "c".'), nl, nl,
		write('e.x. Word="trees",'), nl,
		write('    Guess="crane" ->'), nl,
		write(' Feedback="ncnni."'), nl, nl,
		manual_wordle(W, G),
		enum_print_all(G);
	(MODE == help -> 
		% TODO.	
		write('=== HELP ==='), nl, nl,
		write('This wordle solver has two modes: "auto.", and "manual."'), nl,
		write('*Note the periods at the end of text in quotes. When entering input into this solver, a period must always end the query.'), nl, nl,
		write('= "auto." mode: ='), nl,
		write('The user will enter a word into the solver and the solver will print out on a new line each guess which was used in attempt to determine the word (if not determined, it will also print so after the guesses).'), nl, nl,
		write('= "manual." mode: ='), nl,
		write('The user will pick a word and the solver will try to guess it in six guesses.  Each guess, the user will give "feedback" on the guessed word: what letters are correct, what are in the wrong positions, what are not included, etc. Choose the "manual." mode for more details on entering feedback.'), nl, nl,
		write('=== END OF HELP ==='), nl,
		write('Exiting...');
		write('Invalid mode: "'), write(MODE), write('", type "help." for help. Exiting...'), nl
	))).
	

% Auto-wordle solver: player chooses a word `TAR`, and the program
% outputs `GUESSES` used to try and guess the word.
% Program automatically checks guess info, instead of having user input (see manual_wordle)
auto_wordle(W, TAR, GUESSES) :-
	CANDS1 = W,

	% Guess 1
	gen_guess(CANDS1, G1),
	(TAR == G1 -> GUESSES = [G1];
	gen_feedback(TAR, G1, I1),
	gen_constraints(G1, I1, A1, B1, C1),
	gen_cands(CANDS1, A1, B1, C1, CANDS2),

	% ===

	% Guess 2
	gen_guess(CANDS2, G2),
	(TAR == G2 -> GUESSES = [G1, G2];
	gen_feedback(TAR, G2, I2),
	gen_constraints(G2, I2, A2, B2, C2),
	gen_cands(CANDS2, A2, B2, C2, CANDS3),

	% Guess 3
	gen_guess(CANDS3, G3),
	(TAR == G3 -> GUESSES = [G1, G2, G3];
	gen_feedback(TAR, G3, I3),
	gen_constraints(G3, I3, A3, B3, C3),
	gen_cands(CANDS3, A3, B3, C3, CANDS4),
	
	% Guess 4
	gen_guess(CANDS4, G4),
	(TAR == G4 -> GUESSES = [G1, G2, G3, G4];
	gen_feedback(TAR, G4, I4),
	gen_constraints(G4, I4, A4, B4, C4),
	gen_cands(CANDS4, A4, B4, C4, CANDS5),

	% Guess 5
	gen_guess(CANDS5, G5),
	(TAR == G5 -> GUESSES = [G1, G2, G3, G4, G5];
	gen_feedback(TAR, G5, I5),
	gen_constraints(G5, I5, A5, B5, C5),
	gen_cands(CANDS5, A5, B5, C5, CANDS6),

	% Guess 6
	gen_guess(CANDS6, G6),
	(TAR == G6 -> GUESSES = [G1, G2, G3, G4, G5, G6];
	GUESSES = [G1, G2, G3, G4, G5, G6, '=== NOT FOUND ===']
	)))))).
	

% Manual-wordle solver: player chooses a word `TAR`, and the program
% outputs `GUESSES` used to try and guess the word.
% Program awaits user input to enter guess feedback, instead of doing it itself (see auto_wordle)
manual_wordle(W, GUESSES) :-  
	CANDS1 = W,

	% Guess 1
	gen_guess(CANDS1, G1),
	concat_list(G1, G1_P), write('1: '), write(G1_P), write('?'), nl,
	read(I1_C), atom_chars(I1_C, I1),
	(I1 == [c,c,c,c,c] -> GUESSES = [G1];
	gen_constraints(G1, I1, A1, B1, C1),
	gen_cands(CANDS1, A1, B1, C1, CANDS2),

	% Guess 2
	gen_guess(CANDS2, G2),
	concat_list(G2, G2_P), write('2: '), write(G2_P), write('?'), nl,
	read(I2_C), atom_chars(I2_C, I2),
	(I2 == [c,c,c,c,c] -> GUESSES = [G1, G2];
	gen_constraints(G2, I2, A2, B2, C2),
	gen_cands(CANDS2, A2, B2, C2, CANDS3),

	% Guess 3
	gen_guess(CANDS3, G3),
	concat_list(G3, G3_P), write('3: '), write(G3_P), write('?'), nl,
	read(I3_C), atom_chars(I3_C, I3),
	(I3 == [c,c,c,c,c] -> GUESSES = [G1, G2, G3];
	gen_constraints(G3, I3, A3, B3, C3),
	gen_cands(CANDS3, A3, B3, C3, CANDS4),
	
	% Guess 4
	gen_guess(CANDS4, G4),
	concat_list(G4, G4_P), write('4: '), write(G4_P), write('?'), nl,
	read(I4_C), atom_chars(I4_C, I4),
	(I4 == [c,c,c,c,c] -> GUESSES = [G1, G2, G3, G4];
	gen_constraints(G4, I4, A4, B4, C4),
	gen_cands(CANDS4, A4, B4, C4, CANDS5),

	% Guess 5
	gen_guess(CANDS5, G5),
	concat_list(G5, G5_P), write('5: '), write(G5_P), write('?'), nl,
	read(I5_C), atom_chars(I5_C, I5),
	(I5 == [c,c,c,c,c] -> GUESSES = [G1, G2, G3, G4, G5];
	gen_constraints(G5, I5, A5, B5, C5),
	gen_cands(CANDS5, A5, B5, C5, CANDS6),

	% Guess 6
	gen_guess(CANDS6, G6),
	concat_list(G6, G6_P), write('6: '), write(G6_P), write('?'), nl,
	read(I6_C), atom_chars(I6_C, I6),
	(I6 == [c,c,c,c,c] -> GUESSES = [G1, G2, G3, G4, G5, G6];
	GUESSES = [G1, G2, G3, G4, G5, G6, '=== NOT FOUND ===']
	)))))).

% === ===

