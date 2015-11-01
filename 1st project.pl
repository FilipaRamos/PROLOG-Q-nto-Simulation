
/*PROLOG Q!NTO SIMULATION*/

/* represents the possible colors */
color(r). 
color(b).
color(g).
color(y).
color(c).
color(clr).

/* represents the possible shapes */
shape('*').
shape('!').
shape('#').
shape('+').
shape('&').
shape(s).

/*return a tile*/
oneTile(X) :- color(C), shape(F), X = tile(C,F).

tile(' ',' ').
tile(C, F) :- color(C), shape(F).

/* Possible Normal Tiles */
tile(r, '*').
tile(r, '!').
tile(r, '#').
tile(r, '+').
tile(r, '&').

tile(b, '*').
tile(b, '!').
tile(b, '#').
tile(b, '+').
tile(b, '&').

tile(g, '*').
tile(g, '!').
tile(g, '#').
tile(g, '+').
tile(g, '&').

tile(y, '*').
tile(y, '!').
tile(y, '#').
tile(y, '+').
tile(y, '&').

tile(c, '*').
tile(c, '!').
tile(c, '#').
tile(c, '+').
tile(c, '&').

/* Possible Special Tiles */

/* Auxiliary Functions */
tileColor(tile(C, F)) :- C.
tileShape(tile(C, F)) :- F.
writetile(tile(C,F)) :- write(' '), print(C), print(F), write(' ').

/* Existent Tiles */
makeDeck(X) :- findall(T, oneTile(T), X). /*, imprimeDeck(0, X). */

imprimeDeck(N, L) :- use_module(library(lists)), nth0(N, L, X), 
				writetile(X), N1 is N+1, imprimeDeck(N1, L).

/* Choose a random color */
randomColor(1, r).
randomColor(2, b).
randomColor(3, g).
randomColor(4, y).
randomColor(5, c).

/* Choose a random shape */
randomShape(1, '*').
randomShape(2, '!').
randomShape(3, '#').
randomShape(4, '+').
randomShape(5, '&').
randomShape(6, s).

/* Generate Hand */
randomHand(0).
randomHand(N, L, L2, C) :- N > 0, random(1, C, R), nth1(R, L, X), 
						delete(L, X, L4), C1 is C-1, 
						append([X], L2, L3), N1 is N-1, randomHand(N1, L4, L3, C1).


/* Hand */
/* makeHand(0, H) :- H = []. */
/* N -> número de cartas da mão */
/* L -> mão anterior */
makeHand(N, L) :- makeDeck(X), randomHand(N, X, L, 36), write(L).

/* Display hand */
displayHand(H) :- write(H).
displayTile(C,S) :- write(C), write(S), write(' | ').


/*  A   B    C  D .... */
fguideLine(N, CC) :- CC==N, write('    ').
fguideLine(N, CC) :- CC < N, CC2 is CC+1, write('    '), L is 65+CC, format('~1c', [L]) , write(' '), fguideLine(N, CC2).

/*   ---- ---- ----  */ 
fHorizontalLine(0).
fHorizontalLine(N) :- N>0 , write('  '), write('----'), N1 is N-1 , fHorizontalLine(N1).

/* |    |    |    |  */
sHorizontalLine(0, []) :- write('|'),write(' ').
sHorizontalLine(N, [L|R]) :- N>0, write(' '),  write('|'), writetile(L), N1 is N-1, sHorizontalLine(N1, R).

/* |----|----|----| */ 
tHorizontalLine(0) :-  write('|').
tHorizontalLine(N) :- N>0, write(' '), write('|'), write('----'), N1 is N-1 , tHorizontalLine(N1).

displayBoardaux([], _C).
displayBoardaux([L1], C) :-  write(C) ,write(' '), length(L1,N1), sHorizontalLine(N1,L1), write(' '), write(C), nl.
displayBoardaux([L1|R], C) :- R \= [], length(L1,N1), C < N1, C2 is C+1, write(C), write(' '), sHorizontalLine(N1, L1), write(' '), write(C), nl, write('  '), tHorizontalLine(N1), nl, displayBoardaux(R, C2).

displayBoard([L1|R]) :-  length(L1,N1), nl,write('  '), fguideLine(N1, 0), nl,write('  '), fHorizontalLine(N1), nl,
						displayBoardaux([L1|R],1),  write('  '), fHorizontalLine(N1), nl, write('  '), fguideLine(N1, 0), nl.


createMatrix(W, H, Matrix) :- listElement(L,W,tile(' ',' ')), listElement(Matrix,H,L). 

/*Main*/
createBoard(W,H) :- repeat, createMatrix(W,H, B), displayBoard(B).

/* Computer - Move Tile */
computerMove.

/* User - Move Tile */ 
numberTiles :- write("How many tiles do you want to play? "), read(N), moveTile(N), X is 5-N, computerMove, randomHand(X).

moveTile(0).
moveTile(N) :- N > 0, write("Choose the tile to play. Color: "), 
			read(C), write("Shape: "), read(S), write("Choose where to place it."),
			read(P), N1 is N-1, moveTile(N1).

/*expand matrix*/

empty_tile(tile(' ',' ')).
expand_matrix_up(Matrix, [L|Matrix]) :- empty_tile( E ), matrix_width(Matrix, W), listElement(L, W, E).
expand_matrix_down(Matrix, NewMatrix) :- empty_tile( E ), matrix_width(Matrix, W), listElement(L, W, E), append(Matrix, [L], NewMatrix).
expand_matrix_left([],[]).
expand_matrix_left([L|Matrix], [NL|NewMatrix]):- empty_tile( E ), append([E], L, NL), expand_matrix_left(Matrix, NewMatrix).
expand_matrix_right([],[]).
expand_matrix_right([L|Matrix], [NL|NewMatrix]):- empty_tile( E ), append(L, [E], NL), expand_matrix_right(Matrix, NewMatrix).
listElement([],0, _X).
listElement([X|Xs], N, X) :- N1 is N - 1,  listElement(Xs, N1, X).

/* Load librarys */
load :- use_module(library(random)), use_module(library(lists)).

/* menu */
logo :- write(' ________________________'), nl,
		write('|                        |'), nl,
		write('|          Q!NTO         |'), nl,
		write('|________________________|'), nl, nl.

menu :- repeat, use_module(library(random)), write('\33\[2J'), nl, logo, write(' ---------- MENU ---------'), nl, nl,
			write('    ----- '), write('1. Play'), write(' -----'), nl, 
			write('    ----- '), write('2. Exit'), write(' -----'), nl, nl,
			write('Write the number of the option followed by a dot.'), nl,
			read(C), C>0, C=<2, number(C), choice(C).

/* Menu Options */
choice(1) :- load, createBoard(5,5), nl, makeHand(5, []).
choice(2) :- abort.
