
/**/
color(r). 
color(b).
color(g).
color(y).
color(c).


shape('*').
shape('!').
shape('#').
shape('+').
shape('&').
shape(s).

/*return a piece*/
onePiece(X) :- color(C), shape(F), X = piece(C,F).

piece(' ',' ').
piece(C, F) :- color(C), shape(F).

writepiece(piece(C,F)) :- write(' '), print(C), print(F), write(' ').

fguideLine(N, CC) :- CC==N, write('    ').
fguideLine(N, CC) :- CC < N, CC2 is CC+1, write('    '), L is 65+CC, format('~1c', [L]) , write(' '), fguideLine(N, CC2).

fHorizontalLine(0).
fHorizontalLine(N) :- N>0 , write('  '), write('----'), N1 is N-1 , fHorizontalLine(N1).

sHorizontalLine(0, [], _CC) :- write('|'),write(' '), write(_CC).
sHorizontalLine(N, [L|R], _CC) :- N>0 , write(_CC), write(' '), write('|'), writepiece(L), N1 is N-1, sHorizontalLine(N1, R, _CC2).

tHorizontalLine(0) :-  write('|').
tHorizontalLine(N) :- N>0, write(' '), write('|'), write('----'), N1 is N-1 , tHorizontalLine(N1).

displayBoardaux([]).
displayBoardaux([L1]) :- length(L1,N1), sHorizontalLine(N1,L1,0), nl.
displayBoardaux([L1|R]) :- R \= [], length(L1,N1), sHorizontalLine(N1, L1,0), nl, tHorizontalLine(N1), nl, displayBoardaux(R).

displayBoard([L1|R]) :-  length(L1,N1), nl, fguideLine(N1, 0), nl, fHorizontalLine(N1), nl,
						displayBoardaux([L1|R]), fHorizontalLine(N1), nl, fguideLine(N1, 0), nl.


createMatrix(W, H, Matrix) :- listElement(L,W,piece(' ',' ')), listElement(Matrix,H,L). 


/*Main*/
createBoard(W,H) :- createMatrix(W,H, B), displayBoard(B).


/*expand matrix*/

empty_piece(piece(' ',' ')).
expand_matrix_up(Matrix, [L|Matrix]) :- empty_piece( E ), matrix_width(Matrix, W), listElement(L, W, E).
expand_matrix_down(Matrix, NewMatrix) :- empty_piece( E ), matrix_width(Matrix, W), listElement(L, W, E), append(Matrix, [L], NewMatrix).
expand_matrix_left([],[]).
expand_matrix_left([L|Matrix], [NL|NewMatrix]):- empty_piece( E ), append([E], L, NL), expand_matrix_left(Matrix, NewMatrix).
expand_matrix_right([],[]).
expand_matrix_right([L|Matrix], [NL|NewMatrix]):- empty_piece( E ), append(L, [E], NL), expand_matrix_right(Matrix, NewMatrix).
listElement([],0, _X).
listElement([X|Xs], N, X) :- N1 is N - 1,  listElement(Xs, N1, X).

