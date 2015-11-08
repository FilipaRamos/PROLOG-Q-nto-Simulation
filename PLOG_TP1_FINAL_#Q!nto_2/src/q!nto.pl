:-use_module(library(lists)).
:-use_module(library(between)).
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

imprimeDeck(N, L) :- nth0(N, L, X), writetile(X), N1 is N+1, imprimeDeck(N1, L).

/* Generate Hand */
randomHand(0, _L, _L2, _C, _L2).
randomHand(N, L, L2, C, H) :- N > 0, random(1, C, R), nth1(R, L, X), 
                                                delete(L, X, L4), C1 is C-1, 
                                                append([X], L2, L3), N1 is N-1, randomHand(N1, L4, L3, C1, H).

writeHand(H) :- write(H).


/* Hand */
/* makeHand(0, H) :- H = []. */
/* N -> número de cartas que faltam na mão */
/* L -> mão anterior */
/* H -> nova mão resultante */
makeHand(N, L, H) :- makeDeck(X), randomHand(N, X, L, 36, H), writeHand(H).

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

/* User - Move Tile */ 
numberTiles(L) :- write('How many tiles do you want to play? '), read(Count), movement(Count, L).

movement(0, []).
movement(N, [Move|L]) :- N > 0, moveTile(C, S, Px, Py), T = tile(C, S), Move = [T, Px, Py],
                                    N1 is N-1, movement(N1, L).

/*REPLACES AN ELEMNT */
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

 move(B, Px, Py, T, Bnew) :- nth1(Px, B, L), I is Py-1, replace(L, I, T, B1),    
                             P is Px-1, replace(B, P, B1, Bnew).

/*ASKS COLER, LETTER, POSITION IN ORDER TO MOVE TILE... */
moveTile(C, S, Px, Py) :- write('Choose the tile to play. DO NOT PUT A DOT IN THE END!!!! Color: '),read_line(_),
                        read_line(X1), name(C, X1), color( C ), write('Shape: '), read_line(X2), name(S, X2), shape(S), write('Choose where to place it. Number? '),
                        read(Px), write('Letter ?'), read(Py).
                        

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

/*Expand 5 tiles in each direction*/
expand_matrix_5left(CC, M, NM).
expand_matrix_5left(CC, M, NM) :- CC > 0, CC < 5, expand_matrix_left(M, NM) ,CC1 is CC+1, expand_matrix_5left(CC1, NM, NM1).

test :- createMatrix(5,5, M), displayBoard(M), expand_matrix_5left(1, M, NM), displayBoard(NM).

/* Get Tiles from the board */
getTile(B, Px, Py, T) :- nth1(Px, B, L), nth1(Py, L, T).

/*BOT*/

/*FUNCOES DE TESTE*/

diplay([]).
diplay([H|T]) :- write(H), write(' '), diplay(T). 

/*Verifies if a Given Tile belongs to the Hand*/

inHand(List, Hand):- sublist(Hand, List, _, _, _), List \= [].
inHandPos(List, Hand):- var(List), sublist(Hand, TileTemp, _, _, _),TileTemp \= [],permutation(TileTemp, Tiles), extract_pos(List, Tiles, _).
inHandPos(List, Hand):- nonvar(List), extract_pos(List, Tiles, _), sublist(Hand, Tiles, _, _, _).
/*Extracs position from a list of moves formed by Tile, Px, Py*/
extract_pos([],[],[]).
extract_pos([Move|Moves], [Tile|Tiles], [Pos|Positions]):- Move = [Tile, X, Y], Pos = [X,Y], !, extract_pos(Moves, Tiles, Positions).

/* Verifies whether it is an empty tile or not */
isnotEmptyTile(N, B) :- nth0(N, B, T), E = tile(' ', ' '), dif(E, T).

/* Counts the tiles on the board that have been played */
nTilesBoard(C, B) :- findall(T, isnotEmptyTile(T, B), X), length(X, C).


/*Dir pode ser 01- UP 0-1- DOWN 10- LEFT -10- RIGHT */
validMovAux(B, [X,Y], _D, _T, [],[],[]) :- \+((length(B, H), nth0(0, B, L), length(L, W) , X>0, X =<H, Y>0, Y=<W)), !.
validMovAux(B, [X,Y], _D, _T, [],[],[]) :- getTile(B, X, Y, Tile), Tile = tile(' ', ' '), !.

validMovAux(B, [X,Y], [DirX,DirY], T, [[X, Y]|Tseen], [S|LS], [C|LC]) :- member([X,Y], T),!, getTile(B, X, Y, Tile), Tile = tile(C, S),
                                                                         NextX is X + DirX,
                                                                         NextY is Y + DirY,
                                                                         validMovAux(B, [NextX,NextY], [DirX,DirY], T, Tseen, LS, LC).
validMovAux(B, [X,Y], [DirX,DirY], T, Tseen, [S|LS], [C|LC]) :- getTile(B, X, Y, Tile), Tile = tile(C, S),
                                                                         NextX is X + DirX,
                                                                         NextY is Y + DirY,
                                                                         validMovAux(B, [NextX,NextY], [DirX,DirY], T, Tseen, LS, LC).


validPosition(B, X, Y):- getTile(B, X, Y, Tile), Tile = tile(' ', ' '), 
                         findall([X1, Y1], (between(-1, 1, X1), between(-1, 1, Y1), X1*Y1=:=0, X1+Y1=\=0), L),
                         hasNeighbour(B, X, Y, L).


aux(_NewBoard, [], [_DirX, _DirY], 0).
aux(NewBoard, [[X,Y]|R], [DirX, DirY], P) :- validMovAux(NewBoard, [X,Y], [DirX,DirY], T, _Tseen1, LS3, LC3), 
                                             nl, write(' Aux: PosX : '), write(X), nl,
                                             nl,  write(' Aux: PosY : '), write(Y), nl,
                                              X1 is X - DirX, Y1 is Y - DirY, 
                                              validMovAux(NewBoard, [X1,Y1], [-DirX,-DirY], T, _Tseen2, LS4, LC4),
                                               write(' Aux: PosX1 : '), write(X1),
                                              write(' Aux: PosY1 : '), write(Y1),
                                              append(LS3, LS4, LS),
                                              append(LC3, LC4, LC),
                                              nl, write(' Funcao aux: LS'), diplay(LS), nl, write(' LC '), diplay(LC), nl,
                                              if((all_same_or_different(LS), all_same_or_different(LC)), 
                                                 (length(LS, P2), 
                                                  if(P2 mod 5 =:= 0, P1 is P2*2, P1 is P2)), P1 is 0),
                                              aux(NewBoard, R, [DirX, DirY], P3), P is P1+P3.
                                              

                                                                        
inBounds(B, X, Y):- length(B, H), nth1(1, B, L), length(L, W), 
                   ((X > 0 , X =< H, Y > 0, Y =< W)).

isEmpty(B, X, Y):- getTile(B, X, Y, Tile2), Tile2 = tile(' ', ' ').

hasNeighbour(_B, _X, _Y, []):-!, fail.
hasNeighbour(B, X, Y, [[Dx, Dy]|_Ds]):- X1 is X + Dx, Y1 is Y + Dy, inBounds(B, X1, Y1), \+isEmpty(B, X1, Y1), !.
hasNeighbour(B, X, Y, [[_Dx, _Dy]|Ds]):- hasNeighbour(B, X, Y, Ds).

validPositions(_, []).
validPositions(B, [FirstMove|OtherMoves]):- FirstMove =[_Tile, X, Y], validPosition(B, X, Y), apply_moves(B, [FirstMove], NewB), validPositions(NewB, OtherMoves).

/*Tests if a Move is valid or not, if it isnt returns a valid one...*/

validMov(B,[H|T], Hand, Pont):- validMovVert(B,[H|T], Hand, Pont1) , validMovHor(B,[H|T], Hand, Pont2) , Pont is Pont1+Pont2, Pont > 0.

validMovVert(B,[H|T], Hand, Pontf) :-
        inHandPos([H|T], Hand),
        extract_pos([H|T], [_FirstTile|_Other], [FirstPosition|Positions]),
        FirstPosition = [X,Y],
        validPositions(B, [H|T]),
        apply_moves(B, [H|T], NewBoard),
        validMovAux(NewBoard, [X,Y], [1,0] , Positions, Tseen1, LS1, LC1),
        X1 is X - 1,
        validMovAux(NewBoard, [X1,Y], [-1,0], Positions, Tseen2, LS2, LC2),
        append(Tseen1, Tseen2, Tseen),
        write('Valid Move: Tseen '), diplay(Tseen), 
        append(LS1, LS2, LS), 
        append(LC1, LC2, LC),
       length(LC, N),
       /*aux(NewBoard, Positions , [0,1], P1),*/ 
        if((Tseen = Positions, all_same_or_different(LS), all_same_or_different(LC)),
        (if(N =:= 1, Pont is 0,
         if(N mod 5 =:= 0, Pont is N *2, Pont is N ))),Pont is 0),
        Pontf is Pont.
     /*  if((Tseen = Positions, all_same_or_different(LS), all_same_or_different(LC)), 
        (if(N =:= 1, PontTemp is 0,
        (if(N mod 5 =:= 0, PontTemp is N *2, PontTemp is N )))), PontTemp is 0),
        if(validMovHor(B,[H|T], Hand, Pont2), Pont is PontTemp + Pont2, Pont is PontTemp).*/
       

validMovHor(B,[H|T], Hand, Pontf) :-
        inHandPos([H|T], Hand),
        extract_pos([H|T], [_FirstTile|_Other], [FirstPosition|Positions]),
        FirstPosition = [X,Y],
        validPositions(B, [H|T]),
        apply_moves(B, [H|T], NewBoard),
        validMovAux(NewBoard, [X,Y], [0,1] , Positions, Tseen1, LS1, LC1),
        Y1 is Y - 1,
     validMovAux(NewBoard, [X,Y1], [0,-1], Positions, Tseen2, LS2, LC2),
        append(Tseen1, Tseen2, Tseen),
        append(LS1, LS2, LS), 
        append(LC1, LC2, LC),
        length(LC, N),
        /*aux(NewBoard, Positions , [0,1], P1), */
       if((Tseen = Positions,  all_same_or_different(LS), all_same_or_different(LC)),
        (if(N =:= 1, Pont is 0,
         if(N mod 5 =:= 0, Pont is N *2, Pont is N ))),Pont is 0),
        Pontf is Pont.
 
/*Tests if in a given List all elements are either all the same or all different*/
all_same_or_different(L):- all_same( L ), !.
all_same_or_different(L):- all_different( L ), !.
all_different([]).
all_different([H|T]):- \+member(H, T), all_different( T ).
all_same([H|T]):- length(T,N), listElement(T, N, H).


/*returns all valid moves...*/
getAllValidMoves(B, Hand, L) :- findall(Move, validMov(B,Move, Hand, _Pont), L).

/*calculates best Move the one that uses more tiles...*/
useMoreTiles([], _BestMove).
useMoreTiles([BestH| BestT], BestMove) :-  length(BestH, N1), length(NewBest, N2), N1>N2, NewBest = BestH,
                                            useMoreTiles(BestT, BestMove).

/*return the best Move the one that uses more tiles...*/
best_Mov(B,Hand, NewBest) :- !, getAllValidMoves(B, Hand, [BestH|BestT]), useMoreTiles([BestH| BestT], NewBest).
 
 /*Applies a List of moves*/
apply_moves(NewBoard, [], NewBoard):- !.
apply_moves(B, [[T,X,Y]|TM], NewBoard) :- move(B, X, Y, T, NB), !,apply_moves(NB,TM, NewBoard).


/* Load librarys */
load :- use_module(library(random)), use_module(library(lists)).

/* menu */
logo :- write('           ________________________'), nl,
                write('          |                        |'), nl,
                write('          |          Q!NTO         |'), nl,
                write('          |________________________|'), nl, nl.

playlogo :- write('           ________________________'), nl,
                write('          |                        |'), nl,
                write('          |          PLAY!         |'), nl,
                write('          |________________________|'), nl, nl.

menu :- load, repeat, write('\33\[2J'), nl, logo, write('           ---------- MENU ---------'), nl, nl,
                        write('              ----- '), write('1. Play'), write(' -----'), nl, 
                        write('             ------ '), write('2. Exit'), write(' ------'), nl, nl,
                        write('Write the number of the option followed by a dot.'), nl,
                        read(C), C>0, C=<2, number(C), choice(C).

menuPlay :- repeat, write('\33\[2J'), nl, playlogo, write('        ---------- PLAY MENU ---------'), nl, nl,
                        write('     ------- '), write('1. Player vs Player'), write(' --------'), nl, 
                        write('    -------- '), write('2. Player vs Computer'), write(' -------'), nl,
                        write('   --------- '), write('3. Computer vs Computer'), write(' ------'), nl,
                        write('    ------------  '), write('4. Go Back'), write('  ------------'), nl,
                        write('     -------------  '), write('5. Exit'), write('  ------------'), nl, nl,
                        write('Write the number of the option followed by a dot.'), nl,
                        read(P), P>0, P=<5, number(P), playOp(P).

/* Menu Options */
choice(1) :- menuPlay.
choice(2) :- abort.

/* Play Options */
playOp(1) :- write('\33\[2J'), nl, write('      --- Player 1 turn! --- '), nl, createBoard(5,5,B), displayBoard(B), nl, 
                        makeHand(18, [], H), numberTiles(L), validMov(B, L, H, Pont), apply_moves(B, L, Bnew), displayBoard(Bnew), write('      --- Player 2 turn! ---').
playOp(2).
playOp(3).
playOp(4) :- menu.
playOp(5) :- abort.

/*BOT*/
bot :- load, createBoard(5,5,B), displayBoard(B), nl, makeHand(5, _L, H), get_best_move(B, H, L),  apply_moves(B, L, NB), displayBoard(NB).
/* Done */
/* verifica se o jogo terminou (se houverem 60 tiles no tabuleiro) */
done(B1, P1) :- getHand(P1, H), isEmptyHand(H).

/* Mensagem */
mensagem :- nl, write(" --- Terminou o jogo! --- "), nl.

/* Ciclo de jogo */
:- dynamic state/2.
main :- 
        repeat,
        retract(state(B1, P1)),
        play(B1, P1, B2, P2),
        assert(state(B2, P2)),
        done(B1,P1),
        mensagem.


/*LIXO é so para efeitos de teste...*/
randomBoard :- load, createBoard(5,5,B), T1 = tile(y,'!'),T2 = tile(g,'!'),T3 = tile(r,'!'), L = [[T1,3,4],[T2,2,4], [T3, 1,4]], 
                apply_moves(B, L, NB), displayBoard(NB), Hand = [T1,T2,T3] , validMov(NB, List, Hand, Pont).

t(List, Pont) :- load, createBoard(5,5,B), T1 = tile(y,'!'),T2 = tile(g,'!'),T3 = tile(r,'!'), L = [[T1,3,4],[T2,2,4], [T3, 1,4]], 
                apply_moves(B, L, NB), displayBoard(NB), Hand = [T1,T2,T3] , !, validMov(NB, List, Hand, Pont), ((List = [[tile(y,!),4,4],[tile(g,!),4,5],[tile(r,!),4,3]])->breakpoint; true).

t3(List, Pont) :- load, createBoard(2,2,B), T1 = tile(y,'!'),T2 = tile(g,'!'),T3 = tile(r,'!'), L = [[T1,1,1],[T2,1,2]], 
                apply_moves(B, L, NB), displayBoard(NB), Hand = [T1,T3] , !, validMov(NB, List, Hand, Pont), ((List = [[tile(y,!),2,2],[tile(r,!),2,1]])->breakpoint; true).


t2(List, Pont) :- load, createBoard(5,5,B), T1 = tile(y,'!'),T2 = tile(g,'!'),T3 = tile(r,'!'), L = [[T1,3,4],[T2,2,4], [T3, 1,4]], 
                apply_moves(B, L, NB), displayBoard(NB), Hand = [T1,T2,T3] , !, validPosition(NB, 1, 1).

diplay([]).
diplay([H|T]) :- write(H), write(' '), diplay(T). 

t4(Lis):- load, createBoard(5,5,B), T1 = tile(y,'!'),T2 = tile(g,'!'),T3 = tile(r,'!'), L = [[T1,3,4],[T2,2,4], [T3, 1,4]], 
                apply_moves(B, L, NB), displayBoard(NB), Hand = [T1,T2,T3], !, getAllValidMoves(B, Hand, Lis), diplay(Lis).










