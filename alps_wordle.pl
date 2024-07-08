% ======= Wordle Application =======

get_processed_words([
	[a,l,i,e,n],
	[a,n,k,l,e],
	[a,p,p,l,e],
	[a,u,d,i,o],
	[b,a,c,k,s],
	[b,r,a,k,e],
	[b,r,a,i,n],
	[b,r,e,a,k],
	[c,o,u,n,t],
	[c,r,a,n,e],
	[c,u,r,s,e],
	[d,a,r,e,s],
	[d,r,u,n,k],
	[e,n,t,e,r],
	[f,l,o,o,r],
	[g,r,a,p,h],
	[h,u,r,t,s]
	% Incomplete
]).  


% Returns if `X` is in a list `Y`:
%`in(X, Y)`
in(X, [X|_]).
in(X, [_|T]) :- in(X, T).


% Returns if `X` is not in a list `Y`:
%`not_in(X, Y)`
not_in(_, []).
not_in(X, [H|T]) :- X \== H, not_in(X, T).


% Deletes the first occurance (if there is one) of an element `X` from list `Y`, producing `R`.
% `del(X, Y, R).`
del(_, [], []).
del(X, [X|Ys], Ys).
del(X, [Y|Ys], [Y|Zs]) :- del(X, Ys, Zs).


% Returns whether characters `A` and `B` are equal
% `chars_equal(A, B).`
chars_equal(A, B) :-
	A == B.

	
% Produces the `INFO` list from the given `GUESS` on the target word `TAR`
% Info key: 0=not-in, 1=wrong-pos, 2=correct
% `get_guess_info(TAR, GUESS, INFO).`
get_guess_info(TAR, GUESS, INFO) :- get_guess_info_h(TAR, TAR, GUESS, INFO).
get_guess_info_h(_, [], [], []).
get_guess_info_h(TAR, [T_H|T_T], [G_H|G_T], INFO) :-
	(chars_equal(T_H, G_H) ->  % If correct
		I1 = 2;
		(in(G_H, TAR) ->  % Or char is in
			I1 = 1;
			I1 = 0)),
	get_guess_info_h(TAR, T_T, G_T, I2),
	INFO = [I1|I2].


% Produces the constraint sets `A`, `B`, and `C` for a given `GUESS`  and `INFO`
% Constraints key: A=CORRECT, B=CONTAINED, C=NOT_CONTAINED
% `constraints_from_guess_info(GUESS, INFO, A, B, C).`
constraints_from_guess_info([], [], [], [], []).
constraints_from_guess_info([G_H|G_T], [I_H|I_T], A, B, C) :-
	constraints_from_guess_info(G_T, I_T, A2, B2, C2),
	(I_H == 0 ->  % Not Contained
		A = [0|A2],
		B = B2,
		C = [G_H|C2];

		(I_H == 1 ->  % Wrong Place
			A = [0|A2],
			B = [G_H|B2],
			C = C2;

			(I_H == 2 ->  % Correct
				A = [G_H|A2],
				B = B2,
				C = C2))).


% Produces a word from the wordset `W` which fits constraints `A`, `B`, and `C`
% `match_word(WORD, W, A, B, C).`
match_word([A, B, C, D, E], W, [CO1, CO2, CO3, CO4, CO5], CONTAINED, NOT_CONTAINED) :- 
	in([A, B, C, D, E], W),

	% Set all correct variables
	(C01 \== 0 -> A = C01),
	(C02 \== 0 -> B = C02),
	(C03 \== 0 -> C = C03),
	(C04 \== 0 -> D = C04),
	(C05 \== 0 -> E = C05),
	
	% Each is not in non-contained set
	not_in(A, NOT_CONTAINED),
	not_in(B, NOT_CONTAINED),
	not_in(C, NOT_CONTAINED),
	not_in(D, NOT_CONTAINED),
	not_in(E, NOT_CONTAINED),

	% All in contained-chars set is contained
	del(A, CONTAINED, C1),
	del(B, C1, C2),
	del(C, C2, C3),
	del(D, C3, C4),
	del(E, C4, C5),
	C5 == [].


% Produces a `GUESS` from the `CANDS` list
% `make_guess(CANDS, GUESS)`
make_guess([H|_], H).  % Temp 


% Auto-wordle solver: player chooses a word `TAR`, and the program
% outputs `GUESSES` used to try and guess the word.
% Program automatically checks guess info, instead of having user input (see manual_wordle)
auto_wordle(TAR, GUESSES) :-
	get_processed_words(CANDS1),

	% Guess 1
	make_guess([[c,r,a,n,e]], G1),
	get_guess_info(TAR, G1, I1),
	constraints_from_guess_info(G1, I1, A1, B1, C1),
	findall(WORD, match_word(WORD, CANDS1, A1, B1, C1), CANDS2),

	% Guess 2
	make_guess(CANDS2, G2),
	get_guess_info(TAR, G2, I2),
	constraints_from_guess_info(G2, I2, A2, B2, C2),
	findall(WORD, match_word(WORD, CANDS2, A2, B2, C2), CANDS3), 

	% Guess 3
	make_guess(CANDS3, G3),
	get_guess_info(TAR, G3, I3),
	constraints_from_guess_info(G3, I3, A3, B3, C3),
	findall(WORD, match_word(WORD, CANDS3, A3, B3, C3), CANDS4),
	
	% Guess 4
	make_guess(CANDS4, G4),
	get_guess_info(TAR, G4, I4),
	constraints_from_guess_info(G4, I4, A4, B4, C4),
	findall(WORD, match_word(WORD, CANDS4, A4, B4, C4), CANDS5),

	% Guess 5
	make_guess(CANDS5, G5),
	get_guess_info(TAR, G5, I5),
	constraints_from_guess_info(G5, I5, A5, B5, C5),
	findall(WORD, match_word(WORD, CANDS5, A5, B5, C5), CANDS6),

	% Guess 6
	make_guess(CANDS6, G6),
	get_guess_info(TAR, G6, I6),
	constraints_from_guess_info(G6, I6, A6, B6, C6),
	findall(WORD, match_word(WORD, CANDS6, A6, B6, C6), _),

	GUESSES = [G1, G2, G3, G4, G5, G6].


% Manual-wordle solver: player chooses a word `TAR`, and the program
% outputs `GUESSES` used to try and guess the word.
% Program awaits user input to validate guess info, instead of doing it itself (see auto_wordle)
auto_wordle(TAR, GUESSES) :-
	get_processed_words(CANDS1),

	% Guess 1
	make_guess([[c,r,a,n,e]], G1),
	write(G1), nl,
	read(I1),
	constraints_from_guess_info(G1, I1, A1, B1, C1),
	findall(WORD, match_word(WORD, CANDS1, A1, B1, C1), CANDS2),

	% Guess 2
	make_guess(CANDS2, G2),
	write(G2), nl,
	read(I2),
	constraints_from_guess_info(G2, I2, A2, B2, C2),
	findall(WORD, match_word(WORD, CANDS2, A2, B2, C2), CANDS3), 

	% Guess 3
	make_guess(CANDS3, G3),
	write(G3), nl,
	read(I3),
	constraints_from_guess_info(G3, I3, A3, B3, C3),
	findall(WORD, match_word(WORD, CANDS3, A3, B3, C3), CANDS4),
	
	% Guess 4
	make_guess(CANDS4, G4),
	write(G4), nl,
	read(I4),
	constraints_from_guess_info(G4, I4, A4, B4, C4),
	findall(WORD, match_word(WORD, CANDS4, A4, B4, C4), CANDS5),

	% Guess 5
	make_guess(CANDS5, G5),
	write(G5), nl,
	read(I5),
	constraints_from_guess_info(G5, I5, A5, B5, C5),
	findall(WORD, match_word(WORD, CANDS5, A5, B5, C5), CANDS6),

	% Guess 6
	make_guess(CANDS6, G6),
	write(G6), nl,
	read(I6),
	constraints_from_guess_info(G6, I6, A6, B6, C6),
	findall(WORD, match_word(WORD, CANDS6, A6, B6, C6), _),

	GUESSES = [G1, G2, G3, G4, G5, G6].


