
/*PROLOG Q!NTO SIMULATION*/

/* represents the possible colors */
color(r). 
color(b).
color(g).
color(y).
color(c).
color(' ').

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

imprimeDeck(N, L) :- nth0(N, L, X), writetile(X), 
				N1 is N+1, imprimeDeck(N1, L).

/* Generate Hand */
randomHand(0, _L, _L2, _C).
randomHand(N, L, L2, C) :- N > 0, random(1, C, R), nth1(R, L, X), 
                        delete(L, X, L4), C1 is C-1, 
                        append([X], L2, L3), N1 is N-1, 
                        write(L3), randomHand(N1, L4, L3, C1).


/* Hand */
/* makeHand(0, H) :- H = []. */
/* N -> número de cartas que faltam na mão */
/* L -> mão anterior */
makeHand(N, L, H) :- makeDeck(X), randomHand(N, X, L, 36).

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


/* Lista de Listas..Cria Board Matrix tudo com espaços vazios...*/
createMatrix(W, H, Matrix) :- listElement(L,W,tile(' ',' ')), listElement(Matrix,H,L). 



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
                        

/*Main*/
randomCentre(T) :- makeDeck(L), length(L,N), random(0, N, Num), nth0(Num, L, T).
createBoard(W,H, Bnew) :- repeat, createMatrix(W,H, B), W1 is W/2, H1 is H/2, W2 is ceiling(W1), H2 is ceiling(H1), randomCentre(T), move(B, H2, W2, T, Bnew).
                                                                                          

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

sameColorLinha(0, _B, _Px, _T).
sameColorLinha(N, B, Px, T) :- tileColor(T, C), getTile(B, Px, N, T1), 
                                                        tileColor(T1, C1), compareColorEq(C, C1), 
                                                        N1 is N-1, sameColorLinha(N1, B, Px, T).

sameColorColuna(0, _B, _Py, _T).
sameColorColuna(N, B, Py, T) :- tileColor(T, C),  getTile(B, N, Py, T1), 
                                                        tileColor(T1, C1), compareColorEq(C, C1), 
                                                        N1 is N-1, sameColorColuna(N1, B, Py, T).

sameColors(N, B, _Px, _Py, T) :- sameColorColuna(N, B, _Py, T).
sameColors(N, B, _Px, _Py, T) :- sameColorLinha(N, B, _Px, T).

/* Different shapes in line or row */
compareShape(S, S1) :- \+ S = S1.
compareShape(_S, ' ').
compareShape(' ', _S1).

difShapeLinha(0, _B, _Px, _T).
difShapeLinha(N, B, Px, T) :- tileShape(T, S), getTile(B, Px, N, T1), 
                                                                tileShape(T1, S1), compareShape(S, S1), 
                                                                N1 is N-1, difShapeLinha(N1, B, Px, T).

difShapeColuna(0, _B, _Py, _T).
difShapeColuna(N, B, Py, T) :- tileShape(T, S), getTile(B, N, Py, T1), 
                                                                tileShape(T1, S1), compareShape(S, S1), 
                                                                N1 is N-1, difShapeColuna(N1, B, Py, T). 

difShapes(N, B, _Px, _Py, T) :- difShapeColuna(N, B, _Py, T).
difShapes(N, B, _Px, _Py, T) :- difShapeLinha(N, B, _Px, T).

/* Same shapes in line or row */
compareShapeEq(S, S1) :- S = S1.
compareShapeEq(_S, ' ').
compareShapeEq(' ', _S1).

sameShapeLinha(0, _B, _Px, _T).
sameShapeLinha(N, B, Px, T) :- tileShape(T, S), getTile(B, Px, N, T1), 
                                                                tileShape(T1, S1), compareShapeEq(S, S1), 
                                                                N1 is N-1, sameShapeLinha(N1, B, Px, T).

sameShapeColuna(0, _B, _Py, _T).
sameShapeColuna(N, B, Py, T) :- tileShape(T, S), getTile(B, N, Py, T1), 
                                                                tileShape(T1, S1), compareShapeEq(S, S1), 
                                                                N1 is N-1, sameShapeColuna(N1, B, Py, T).

sameShapes(N, B, _Px, _Py, T) :- sameShapeColuna(N, B, _Px, _Py, T).
sameShapes(N, B, _Px, _Py, T) :- sameShapeLinha(N, B, _Px, _Py, T).
sameShapes(N, B, _Px, _Py, T) :- sameShapeLinha(N, B, _Px, _Py, T), sameShapeColuna(N, B, _Px, _Py, T).


/* All possible valid moves */
validMove(N, B, Px, Py, T) :- difColors(N, B, Px, Py, T), sameShapes(N, B, Px,Py,T) .
validMove(N, B, Px, Py, T) :- sameColors(N, B, Px, Py, T), difShapes(N, B, Px, Py, T).
validMove(N, B, Px, Py, T) :- difShapes(N, B, Px, Py, T), difColors(N, B, Px, Py, T).
validMove(N, B, Px, Py, T) :- sameShapes(N, B, Px, Py, T), sameColors(N, B, Px, Py, T).

/*BOT*/

/*Verifies is a Given Tile belongs to the Hand*/
inHand([], _Hand).
inHand([H|T], Hand) :- member(H, Hand), inHand(T, Hand). 


/* FUNCOES DE VALID ERA A IDEIA QUE TIVE ONTEM, MAS ALTERA AS TUAS QUE DEPOIS ALTERO O MEU BOT... ESTAS NÃO ESTAO TESTADAS... */

/*Dir pode ser 01- UP 0-1- DOWN 10- LEFT -10- RIGHT */
validMovAux(B, [X,Y], _D, _T, [],[],[]) :- \+((length(B, H), nth0(0, B, L), length(L, W) , X>=0, X < W, Y>=0, Y<H)), !.
validMovAux(B, [X,Y], _D, _T, [],[],[]) :- getTile(B, X, Y, Tile), Tile = tile(' ', ' '), !.

validMovAux(B, [X,Y], [DirX,DirY], T, [[X, Y]|Tseen], [S|LS], [C|LC]) :- member([X,Y], T),!, getTile(B, X, Y, Tile), Tile = tile(C, S),
                                                                         NextX is X + DirX,
                                                                         NextY is Y + DirY,
                                                                         validMovAux(B, [NextX,NextY], [DirX,DirY], T, Tseen, LS, LC).
validMovAux(B, [X,Y], [DirX,DirY], T, Tseen, [S|LS], [C|LC]) :- getTile(B, X, Y, Tile), Tile = tile(C, S),
                                                                         NextX is X + DirX,
                                                                         NextY is Y + DirY,
                                                                         validMovAux(B, [NextX,NextY], [DirX,DirY], T, Tseen, LS, LC).


/*Tests if a Move is valid or not, if it isnt returns a valid one...*/

validMov(B,[H|T], Hand, Pont) :- H = [P,X,Y], 
        inHand(P, Hand),
        validMovAux(B, [X,Y], [1,0] ,T, Tseen1, LS1, LC1),
        X1 is X - 1,
        validMovAux(B, [X1,Y], [-1,0],T, Tseen2, LS2, LC2),
        append(Tseen1, Tseen2, Tseen),
        Tseen = T,
        append(LS1, LS2, LS), 
        append(LC1, LC2, LC),
        all_same_or_different(LS),
        all_same_or_different(LC), 
        length(LS, N),
        if(N mod 5 =:= 0, Pont is N *2, Pont is N ).

validMov(B,[H|T], Hand, Pont) :- H = [P,X,Y],
        inHand(P, Hand),
        validMovAux(B, [X,Y], [0,1] ,T, Tseen3, LS3, LC3),
        Y1 is Y - 1,
        validMovAux(B, [X,Y1], [0,-1] ,T, Tseen4, LS4, LC4),
        append(Tseen3, Tseen4, Tseen),
        Tseen = T,
        append(LS3, LS4, LS),
        append(LC3, LC4, LC),
        all_same_or_different(LS),
        all_same_or_different(LC),
        length(LC, N),
        Pont is N.

/*Tests if in a given List are either all the same or all different*/
all_same_or_different(L):- all_same( L ), !.
all_same_or_different(L):- all_different( L ), !.
all_different([]).
all_different([H|T]):- \+member(H, T), all_different( T ).
all_same([H|T]):- length(T,N), listElement(T, N, H).


/*returns all valid moves...*/
grtAllValidMoves(B,[P,X,Y|T], Hand, Pont, L) :- findall([P,X,Y],validMov(B,[P,X,Y|T], Hand, Pont), L).

best_Mov(B,[P,X,Y|T], Hand, Pont, [BestH|BestT], NewBest) :- grtAllValidMoves(B,[P,X,Y|T], Hand, Pont, [BestH|BestT]), 
                                                    length(BestH, N1), legnth(NewBest, N2), N1 > N2, NewBest = BestH,  
                                                    best_Mov(B,[P,X,Y|T], Hand, Pont, BestT, NewBest).


get_best_move(B, Hand,L) :- best_Mov(B,[_P,_X,_Y|_T], Hand, _Pont, [_BestH|_BestT], L).


/*Applies a List of moves*/
apply_moves(NewBoard, [], NewBoard).
apply_moves(B, [[T,X,Y]|TM], NewBoard) :- move(B, X, Y, T, NB), apply_moves(NB,TM, NewBoard).





/* Load librarys */
load :- use_module(library(random)), use_module(library(lists)).

/* menu */
logo :- write(' ________________________'), nl,
                write('|                        |'), nl,
                write('|          Q!NTO         |'), nl,
                write('|________________________|'), nl, nl.

start :- load, createBoard(5,5,B), displayBoard(B), nl, makeHand(5, []).

menu :- repeat, write('\33\[2J'), nl, logo, write(' ---------- MENU ---------'), nl, nl,
                        write('    ----- '), write('1. Play'), write(' -----'), nl, 
                        write('    ----- '), write('2. Exit'), write(' -----'), nl, nl,
                        write('Write the number of the option followed by a dot.'), nl,
                        read(C), C>0, C=<2, number(C), choice(C).

/* Menu Options */
choice(1) :- start.
choice(2) :- abort.


/*LIXO é so para efeitos de teste...*/
randomBoard :- load, createBoard(5,5,B), T1 = tile(y,'!'),T2 = tile(g,'!'),T3 = tile(r,'!'), L = [[T1,3,4],[T2,2,4], [T3, 1,4]], apply_moves(B, L, NB), displayBoard(NB).

