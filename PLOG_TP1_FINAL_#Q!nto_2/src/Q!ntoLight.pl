:-use_module(library(lists)).
:-use_module(library(between)).
:- use_module(library(random)).
/*PROLOG Q!NTO SIMULATION*/

/* represents the possible colors */
color(r). 
color(b).
color(g).
color(y).
color(' ').
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

/*
///////////////////////////////////////
///////////////  DECK  ////////////////
///////////////////////////////////////
*/

/*Creating Deck*/

wildQuintoTile(C, S) :- C = c, S = s.

wildColorTile(C, _S) :- C = c.
wildShapeTile(_C, S) :- S = s. 

deckWithDuplicates(X) :- findall(T, oneTile(T), X1), findall(T, oneTile(T), X2), append(X1, X2, X).

rmvElem(NewDeck, [], NewDeck).
rmvElem(Deck, [LElem|T], NewDeck) :- delete(Deck, LElem, Deck1), rmvElem(Deck1, T, NewDeck).


remvWildTiles(Deck, NewDeck) :- CT1 = tile(c, '*'), CT2 = tile(c, '#'), CT3 = tile(c, '+'),  CT4 = tile(c, '&'),  
                                ST1 = tile(r, s), ST2 = tile(g, s), ST3 = tile(b, s), ST4 = tile(y, s), ST5 = tile(c, s),
                                ExclamationTile = tile(c,'!'), BlackTile = tile(' ',s),
                                L = [CT1, CT2, CT3, CT4, ST1, ST2, ST3, ST4, ST5, ExclamationTile, BlackTile], rmvElem(Deck, L, NewDeck).


makeDeck(Deck) :- deckWithDuplicates(L1),  remvWildTiles(L1, L2), 
                  CT1 = tile(c, '*'), CT2 = tile(c, '#'), CT3 = tile(c, '+'),  CT4 = tile(c, '&'),  CT5 = tile(c, s), 
                  ST1 = tile(r, s), ST2 = tile(g, s), ST3 = tile(b, s), ST4 = tile(y, s), ST5 = tile(c, s),
                  L = [CT1, CT2, CT3, CT4, CT5, ST1, ST2,ST3,ST4,ST5], append(L2, L, Deck).

/*
///////////////////////////////////////
///////////////  HAND  ////////////////
///////////////////////////////////////
*/
                  
/*Creating Hands - Mixing Deck and Dividing it in two hands*/

mixingElemtsDeck(Deck, NewDeck) :- makeDeck(Deck), random_permutation(Deck, NewDeck).

div(L, A, B) :-
    append(A, B, L),
    length(A, N),
    length(B, N).

creatingHand(Deck, Hand1, Hand2) :- div(Deck, Hand1, Hand2).




/* Display hand */
displayHand(H) :- write(H).
displayTile(C,S) :- write(C), write(S), write(' | ').

/*
///////////////////////////////////////
///////////BOARD - DISPLAY/////////////
///////////////////////////////////////
*/

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
displayBoardaux([L1], C) :- write('  '), length(L1,N1), sHorizontalLine(N1,L1), write(' '), write(C), nl.
displayBoardaux([L1|R], C) :- R \= [], length(L1,N1), C < N1, C2 is C+1, write('  '), sHorizontalLine(N1, L1), write(' '), write(C), nl, write('  '), tHorizontalLine(N1), nl, displayBoardaux(R, C2).

displayBoard([L1|R]) :-  length(L1,N1), nl,write('  '), fguideLine(N1, 0), nl,write('  '), fHorizontalLine(N1), nl,
                                                displayBoardaux([L1|R],1),  write('  '), fHorizontalLine(N1), nl, write('  '), fguideLine(N1, 0), nl.

/*
///////////////////////////////////////
///////////BOARD - CREATION////////////
///////////////////////////////////////
*/

/* Lista de Listas..Cria Board Matrix tudo com espaços vazios...*/
createBoard(W, H, Matrix) :- listElement(L,W,tile(' ',' ')), listElement(Matrix,H,L). 

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
expand_matrix_5left(5, NM, NM1).
expand_matrix_5left(CC, M, NM) :- CC < 5, CC > 0, expand_matrix_left(M, NM),CC1 is CC+1, expand_matrix_5left(CC1, NM, NM1).

/*
//////////////////////////////////////////////////////
//////////////////MOVEMENT FUNCTIONS//////////////////
//////////////////////////////////////////////////////
*/

deleteElemHand(Hand, Tile, NewHand) :- delete(Hand,Tile,NewHand1), length(NewHand1,TamanhoDepois), length(Hand,TamanhoAntes), TamanhoAntes-TamanhoDepois=:=2,append(NewHand1, Tile, NewHand).
deleteElemHand(Hand, Tile, NewHand) :- delete(Hand,Tile,NewHand1).

/*REPLACES AN ELEMNT */
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

move(B, Px, Py, T, Hand, Bnew) :- nth1(Px, B, L), I is Py-1, replace(L, I, T, B1),    
                             P is Px-1, replace(B, P, B1, Bnew), deleteElemHand(Hand, T, NewHand).
                             
/*Applies a List of moves*/
apply_moves(NewBoard, [], Hand, NewBoard):- !.
apply_moves(B, [[T,X,Y]|TM], Hand, NewBoard) :- move(B, X, Y, T, Hand, NB), !,apply_moves(NB,TM, NewHand, NewBoard).

/*
//////////////////////////////////////
///////////VALID-MOV//////////////////
//////////////////////////////////////
*/

inBounds(B, X, Y):- length(B, H), nth1(1, B, L), length(L, W), 
                   ((X > 0 , X =< H, Y > 0, Y =< W)).

isEmpty(B, X, Y):- getTile(B, X, Y, Tile2), Tile2 = tile(' ', ' ').

hasNeighbour(_B, _X, _Y, []):-!, fail.
hasNeighbour(B, X, Y, [[Dx, Dy]|_Ds]):- X1 is X + Dx, Y1 is Y + Dy, inBounds(B, X1, Y1), \+isEmpty(B, X1, Y1), !.
hasNeighbour(B, X, Y, [[_Dx, _Dy]|Ds]):- hasNeighbour(B, X, Y, Ds).

validPositions(_, []).
validPositions(B, [FirstMove|OtherMoves]):- FirstMove =[_Tile, X, Y], validPosition(B, X, Y), apply_moves(B, [FirstMove], NewB), validPositions(NewB, OtherMoves).

/*Verifies if a Given Tile belongs to the Hand*/

inHand(List, Hand):- sublist(Hand, List, _, _, _), List \= [].
inHandPos(List, Hand):- var(List), sublist(Hand, TileTemp, _, _, _),TileTemp \= [],permutation(TileTemp, Tiles), extract_pos(List, Tiles, _).
inHandPos(List, Hand):- nonvar(List), extract_pos(List, Tiles, _), sublist(Hand, Tiles, _, _, _).

/*Extracs position from a list of moves formed by Tile, Px, Py*/
extract_pos([],[],[]).
extract_pos([Move|Moves], [Tile|Tiles], [Pos|Positions]):- Move = [Tile, X, Y], Pos = [X,Y], !, extract_pos(Moves, Tiles, Positions).


/*Tests if a Move is valid or not, if it isnt returns a valid one...*/
validMov(B,[H|T], Hand):- validMovVert(B,[H|T], Hand) , validMovHor(B,[H|T], Hand).

validMovVert(B,[H|T], Hand) :-
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
       (Tseen = Positions, all_same_or_different(LS), all_same_or_different(LC)).
    
       

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
       (Tseen = Positions,  all_same_or_different(LS), all_same_or_different(LC)).
 
 
/*Tests if in a given List all elements are either all the same or all different*/
all_same_or_different(L):- all_same( L ), !.
all_same_or_different(L):- all_different( L ), !.
all_different([]).
all_different([H|T]):- \+member(H, T), all_different( T ).
all_same([H|T]):- length(T,N), listElement(T, N, H).
                  

/*
/////////////////////////////////
///////BOT2 - SMART//////////////
/////////////////////////////////
*/

/*returns all valid moves...*/
getAllValidMoves(B, Hand, L) :- findall(Move, validMov(B,Move, Hand, _Pont), L).

/*calculates best Move the one that uses more tiles...*/
useMoreTiles([], _BestMove).
useMoreTiles([BestH| BestT], BestMove) :-  length(BestH, N1), length(NewBest, N2), N1>N2, NewBest = BestH,
                                            useMoreTiles(BestT, BestMove).

/*return the best Move the one that uses more tiles...*/
best_Mov(B,Hand, NewBest) :- !, getAllValidMoves(B, Hand, [BestH|BestT]), useMoreTiles([BestH| BestT], NewBest).

/*
///////////////////////////////////////////////////////////////////////
///////////////////////////PLAYER1 VS PLAYER2//////////////////////////
///////////////////////////////////////////////////////////////////////
*/






/*FUNCOES DE TESTE*/

diplay([]).
diplay([H|T]) :- write(H), nl, diplay(T).

test :- createBoard(5,5, M), displayBoard(M), expand_matrix_5left(1, M, NM), displayBoard(NM).


randomBoard :- load, createBoard(5,5,B), T1 = tile(y,'!'),T2 = tile(g,'!'),T3 = tile(r,'!'), L = [[T1,3,4],[T2,2,4], [T3, 1,4]], 
                apply_moves(B, L, NB), displayBoard(NB), Hand = [T1,T2,T3] , validMov(NB, List, Hand, Pont).

t(List) :- load, createBoard(5,5,B), T1 = tile(y,'!'),T2 = tile(g,'!'),T3 = tile(r,'!'), L = [[T1,3,4],[T2,2,4], [T3, 1,4]], 
                apply_moves(B, L, NB), displayBoard(NB), Hand = [T1,T2,T3] , !, validMov(NB, List, Hand), ((List = [[tile(y,!),4,4],[tile(g,!),4,5],[tile(r,!),4,3]])->breakpoint; true).

t3(List, Pont) :- load, createBoard(2,2,B), T1 = tile(y,'!'),T2 = tile(g,'!'),T3 = tile(r,'!'), L = [[T1,1,1],[T2,1,2]], 
                apply_moves(B, L, NB), displayBoard(NB), Hand = [T1,T3] , !, validMov(NB, List, Hand, Pont), ((List = [[tile(y,!),2,2],[tile(r,!),2,1]])->breakpoint; true).


t2(List, Pont) :- load, createBoard(5,5,B), T1 = tile(y,'!'),T2 = tile(g,'!'),T3 = tile(r,'!'), L = [[T1,3,4],[T2,2,4], [T3, 1,4]], 
                apply_moves(B, L, NB), displayBoard(NB), Hand = [T1,T2,T3] , !, validPosition(NB, 1, 1).

t4(Lis):- load, createBoard(5,5,B), T1 = tile(y,'!'),T2 = tile(g,'!'),T3 = tile(r,'!'), L = [[T1,3,4],[T2,2,4], [T3, 1,4]], 
                apply_moves(B, L, NB), displayBoard(NB), Hand = [T1,T2,T3], !, getAllValidMoves(B, Hand, Lis), diplay(Lis).
