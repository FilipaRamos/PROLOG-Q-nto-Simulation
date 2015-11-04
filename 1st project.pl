
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

/* Auxiliary Functions */
tileColor(tile(C, _F), C).
tileShape(tile(_C, F), F).
writetile(tile(C,F)) :- write(' '), print(C), print(F), write(' ').

/* Existent Tiles */
makeDeck(X) :- findall(T, oneTile(T), X).

imprimeDeck(N, L) :- use_module(library(lists)), nth0(N, L, X), 
				writetile(X), N1 is N+1, imprimeDeck(N1, L).

/* Generate Hand */
randomHand(0, _L, _L2, _C).
randomHand(N, L, L2, C) :- N > 0, random(1, C, R), nth1(R, L, X), 
						delete(L, X, L4), C1 is C-1, 
						append([X], L2, L3), N1 is N-1, write(L3), randomHand(N1, L4, L3, C1).


/* Hand */
/* makeHand(0, H) :- H = []. */
/* N -> número de cartas da mão */
/* L -> mão anterior */
makeHand(N, L) :- makeDeck(X), randomHand(N, X, L, 36).

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


createMatrix(W, H, Matrix) :-   W2 is W, W1 is W//2, W2 > W1, listElement(L,W2,tile(' ',' ')), H2 is H, H1 is H//2, H2 > H1, listElement(Matrix,H2,L).
createMatrix(_W, _H, _Matrix) :- listElement(_L,_W1,tile(c,'!')). 
createMatrix(W, H, Matrix) :- W2 is W, W1 is W//2, W2 < W1, listElement(L,W2,tile(' ',' ')), H2 is H, H1 is H//2, H2 < H1, listElement(Matrix,H2,L). 
/* Cria Board Matrix tudo com espaços vazios...*/

/*Main*/
createBoard(W,H) :- repeat, createMatrix(W,H, B), displayBoard(B).

/* Computer - Move Tile */
computerMove.

/* User - Move Tile */ 
numberTiles :- write('How many tiles do you want to play? '), read(N), moveTile(N), X is 5-N, computerMove, randomHand(X).

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).


move(B, Px, Py, T, Bnew) :- nth1(Px, B, L), I is Py-1, replace(L, I, T, B1), 
						P is Px-1, replace(B, P, B1, Bnew).


moveTile(_C, _S, _Px, _Py) :- write('Choose the tile to play. Color: '), 
			read(_C), write('Shape: '), read(_S), write('Choose where to place it. Number? '),
			read(_Px), write('Letter ?'), read(_Py).
			
/* Set list element to Elem*/
list_get(L, Index, Elem) :- nth0(Index, L, Elem). /* retorna index a alterar*/
list_set([], 0, _Elem, [_NH|_NT]).
list_set([_H|T], Index, Elem, [_NH|NT]) :- list_get(_L, Index, Elem), Index > -1, N1 is Index-1, list_set(T, N1,Elem, NT). 
list_set(L, _, _, L).

/*Set matrix element to Elem in position Px, Py*/
matrix_set(Matrix, Px, Py, Elem, NewMatrix) :- list_get(Matrix, Py, L),  list_set(L, Px, Elem, L2), list_set(Matrix, _Y, L2, NewMatrix).


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

/* Get Tiles from the board */
getTile(B, Px, Py, T) :- nth1(Px, B, L), nth1(Py, L, T).

/* Valid Moves */

/* Different colors in line or row */
compareColor(C, C1) :- \+ C = C1.
compareColor(_C, ' ').
compareColor(' ', _C).

difColorLinha(0, _B, _Px, _T).
difColorLinha(N, B, Px, T) :- tileColor(T, C), getTile(B, Px, N, T1), 
							tileColor(T1, C1), compareColor(C, C1),
							N1 is N-1, difColorLinha(N1, B, Px, T).

difColorColuna(0, _B, _Py, _T).
difColorColuna(N, B, Py, T) :- tileColor(T, C), getTile(B, N, Py, T1), 
							tileColor(T1, C1), compareColor(C, C1), 
							N1 is N-1, difColorColuna(N1, B, Py, T).

difColors(N, B, _Px, _Py, T) :- difColorColuna(N, B, _Py, T).
difColors(N, B, _Px, _Py, T) :- difColorLinha(N, B, _Px, T).

/* Same colors in line or row */
compareColorEq(C, C1) :- C = C1.
compareColorEq(_C, ' ').
compareColorEq(' ', _C).

sameColorLinha(0, _L, _B, _Px, _Py, _T).
sameColorLinha(N, L, B, Px, Py, T) :- P is Py+1, tileColor(T, C),
							getTile(B, Px, P, T1), tileColor(T1, C1),
							compareColorEq(C, C1), N1 is N-1, sameColorLinha(N1, L, B, Px, P, T).
sameColorLinha(N, L, B, Px, Py, T) :- _P0 is Py-1, tileColor(T, C),
							getTile(B, Px, P, T1), tileColor(T1, C1),
							compareColorEq(C, C1), N1 is N-1, sameColorLinha(N1, L, B, Px, P, T).

sameColorColuna(0, _L, _B, _Px, _Py, _T).
sameColorColuna(N, L, B, Px, Py, T) :- P is Px+1, tileColor(T, C), 
							getTile(B, P, Py, T1), tileColor(T1, C1),
							compareColorEq(C, C1), N1 is N-1, sameColorColuna(N1, L, B, P, Py, T).
sameColorColuna(N, L, B, Px, Py, T) :- P is Px-1, tileColor(T, C), 
							getTile(B, P, Py, T1), tileColor(T1, C1),
							compareColorEq(C, C1), N1 is N-1, sameColorColuna(N1, L, B, P, Py, T).

sameColors(N, L, B, Px, Py, T) :- sameColorColuna(N, L, B, Px, Py, T).
sameColors(N, L, B, Px, Py, T) :- sameColorLinha(N, L, B, Px, Py, T).

/* Different shapes in line or row */
compareShape(S, S1) :- \+ S = S1.
compareShape(_S, ' ').
compareShape(' ', _S1).

difShapeLinha(0, _L, _B, _Px, _Py, _T).
difShapeLinha(N, L, B, Px, Py, T) :- P is Py+1, tileShape(T, S),
								getTile(B, Px, P, T1), tileShape(T1, S1),
								compareShape(S, S1), N1 is N-1, difShapeLinha(N1, L, B, Px, P, T).
difShapeLinha(N, L, B, Px, Py, T) :- P is Py-1, tileShape(T, S),
								getTile(B, Px, P, T1), tileShape(T1, S1),
								compareShape(S, S1), N1 is N-1, difShapeLinha(N1, L, B, Px, P, T).

difShapeColuna(0, _L, _B, _Px, _Py, _T).
difShapeColuna(N, L, B, Px, Py, T) :- P is Px+1, tileShape(T, S),
								getTile(B, P, Py, T1), tileShape(T1, S1),
								compareShape(S, S1), N1 is N-1, difShapeColuna(N1, L, B, P, Py, T).
difShapeColuna(N, L, B, Px, Py, T) :- P is Px-1, tileShape(T, S),
								getTile(B, P, Py, T1), tileShape(T1, S1),
								compareShape(S, S1), N1 is N-1, difShapeColuna(N1, L, B, P, Py, T). 

difShapes(N, L, B, Px, Py, T) :- difShapeColuna(N, L, B, Px, Py, T).
difShapes(N, L, B, Px, Py, T) :- difShapeLinha(N, L, B, Px, Py, T).

/* Same shapes in line or row */
compareShapeEq(S, S1) :- S = S1.
compareShapeEq(_S, ' ').
compareShapeEq(' ', _S1).

sameShapeLinha(0, _L, _B, _Px, _Py, _T).
sameShapeLinha(N, L, B, Px, Py, T) :- P is Py+1, tileShape(T, S),
								getTile(B, Px, P, T1), tileShape(T1, S1),
								compareShapeEq(S, S1), N1 is N-1, sameShapeLinha(N1, L, B, Px, P, T).
sameShapeLinha(N, L, B, Px, Py, T) :- P is Py-1, tileShape(T, S),
								getTile(B, Px, P, T1), tileShape(T1, S1),
								compareShapeEq(S, S1), N1 is N-1, sameShapeLinha(N1, L, B, Px, P, T).

sameShapeColuna(0, _L, _B, _Px, _Py, _T).
sameShapeColuna(N, L, B, Px, Py, T) :- P is Px+1, tileShape(T, S),
								getTile(B, P, Py, T1), tileShape(T1, S1),
								compareShapeEq(S, S1), N1 is N-1, sameShapeColuna(N1, L, B, P, Py, T).
sameShapeColuna(N, L, B, Px, Py, T) :- P is Px-1, tileShape(T, S),
								getTile(B, P, Py, T1), tileShape(T1, S1),
								compareShapeEq(S, S1), N1 is N-1, sameShapeColuna(N1, L, B, P, Py, T). 

sameShapes(N, L, B, Px, Py, T) :- sameShapeColuna(N, L, B, Px, Py, T).
sameShapes(N, L, B, Px, Py, T) :- sameShapeLinha(N, L, B, Px, Py, T).

/* All possible valid moves */
validMove(N, L, B, Px, Py, T) :- difColors(N, L, B, Px, Py, T).
validMove(N, L, B, Px, Py, T) :- sameColors(N, L, B, Px, Py, T).
validMove(N, L, B, Px, Py, T) :- difShapes(N, L, B, Px, Py, T).
validMove(N, L, B, Px, Py, T) :- sameShapes(N, L, B, Px, Py, T).


/* Load librarys */
load :- use_module(library(random)), use_module(library(lists)).

/* menu */
logo :- write(' ________________________'), nl,
		write('|                        |'), nl,
		write('|          Q!NTO         |'), nl,
		write('|________________________|'), nl, nl.

start :- load, createBoard(5,5), nl, makeHand(5, []).

menu :- repeat, use_module(library(random)), write('\33\[2J'), nl, logo, write(' ---------- MENU ---------'), nl, nl,
			write('    ----- '), write('1. Play'), write(' -----'), nl, 
			write('    ----- '), write('2. Exit'), write(' -----'), nl, nl,
			write('Write the number of the option followed by a dot.'), nl,
			read(C), C>0, C=<2, number(C), choice(C).

/* Menu Options */
choice(1) :- start.
choice(2) :- abort.
