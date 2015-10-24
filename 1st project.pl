
/*PROLOG Q!NTO SIMULATION*/

/* LIBRARYS */
use_module(library(random)).

/* represents the possible colors */
color(r). 
color(b).
color(g).
color(y).
color(c).

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

writetile(tile(C,F)) :- write(' '), print(C), print(F), write(' ').

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

/* Initial Hand */
hand :- {}.

/* Add Tile to Hand */
addHandTile(C, [H|T]) :- append(C, [H|T], hand). /* precisa de ser corrigido */

/* Generate Hand */
randomHand(0).
randomHand(N) :- N>0, random(1, 5, R1), randomColor(R1, C), 
			randomShape(1, 6, R2), randomShape(R2, S),
			addHandTile(tile(C, S), hand, hand),
			N1 is N+1, randomHand(N1).

/* Choose a random color */
randomColor(1, color(r)).
randomColor(2, color(b)).
randomColor(3, color(g)).
randomColor(4, color(y)).
randomColor(5, color(c)).

/* Choose a random shape */
randomShape(1, shape('*')).
randomShape(2, shape('!')).
randomShape(3, shape('#')).
randomShape(4, shape('+')).
randomShape(5, shape('&')).
randomShape(6, shape(s)).

/* Display hand */
displayHand([H|T]) :- write(H), write('   '), displayHand(T).

/*Main*/
createBoard(W,H) :- repeat, createMatrix(W,H, B), displayBoard(B).

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

/* menu */
menu :- repeat, write('      MENU     '), nl,
			write('1. Play'), nl, write('2. Exit'), nl, 
			write('Write the number of the option followed by a dot.'), nl,
			read(C), C>0, C=<2, choice(C).

/* Menu Options */
choice(1) :- createBoard(5,5).
choice(2) :- abort.
