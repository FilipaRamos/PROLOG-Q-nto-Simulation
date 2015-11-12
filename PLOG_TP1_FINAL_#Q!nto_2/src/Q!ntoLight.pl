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


%//////////////////////////////////////////////////////////////  DECK  /////////////////////////////////////////////////////////////////////////


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

%//////////////////////////////////////////////////////////////////////  HAND  ////////////////////////////////////////////////////////////////////////////////
                 
/*Creating Hands - Mixing Deck and Dividing it in two hands*/

mixingElemtsDeck(Deck, NewDeck) :- random_permutation(Deck, NewDeck).

div(L, A, B) :-
    append(A, B, L),
    length(A, N),
    length(B, N).

creatingHand(Deck, Hand1, Hand2) :- div(Deck, Hand1, Hand2).

/* Display hand */

displayHand([_HHand|_T], 5).
displayHand([HHand|T], CC) :- CC < 5, CC1 is CC+1, write(HHand), write('  '), displayHand(T, CC1).

displayTile(C,S) :- write(C), write(S), write(' | ').


%///////////////////////////////////////////////////////////////////////BOARD - DISPLAY/////////////////////////////////////////////////////////////////////////

writetile(tile(C,F)) :- write(' '), print(C), print(F), write(' ').
/*  1   2    3  4 .... */
fguideLine(N, CC) :- CC >= N, write('    ').
fguideLine(N, CC) :- CC < N, CC2 is CC+1, write('    '), write(CC2) , write(' '), fguideLine(N, CC2).

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


%////////////////////////////////////////////////////////////////////BOARD - CREATION///////////////////////////////////////////////////////////////////////////

/* Lista de Listas..Cria Board Matrix tudo com espaços vazios...*/
createBoard(W, H, Matrix) :- listElement(L,W,tile(' ',' ')), listElement(Matrix,H,L). 

randomCentre(T, Hand) :- length(Hand,N), random(0, N, Num), nth0(Num, Hand, T).
createCenter(B, W, H, Hand, Bnew, NHand) :-  W1 is W/2, H1 is H/2, W2 is ceiling(W1), H2 is ceiling(H1), randomCentre(T, Hand),
                                        move(B, H2, W2, T, Bnew), deleteElemHand(Hand, T, NHand).

/*expand matrix*/
matrix_width(Matrix, W) :- nth0(0, Matrix, Elem), length(Elem, W).
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
expand_matrix_5left(5, _NM, _NM1).
expand_matrix_5left(CC, M, NM) :- CC < 5, CC > 0, expand_matrix_left(M, NM),CC1 is CC+1, expand_matrix_5left(CC1, NM, _NM1).

/* Get Tiles from the board */
getTile(B, Px, Py, T) :- nth1(Px, B, L), nth1(Py, L, T).

%//////////////////////////////////////////////////////////////////////MOVEMENT FUNCTIONS////////////////////////////////////////////////////////////

deleteElemHand(Hand, Tile, NewHand):-select(Tile, Hand, NewHand), !.
deleteElemHand(Hand, _Tile, Hand).
/*REPLACES AN ELEMNT */
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

move(B, Px, Py, T, Hand, Bnew, NewHand) :- nth1(Px, B, L), I is Py-1, replace(L, I, T, B1),    
                             P is Px-1, replace(B, P, B1, Bnew), deleteElemHand(Hand, T, NewHand).

move(B, Px, Py, T, Bnew) :- nth1(Px, B, L), I is Py-1, replace(L, I, T, B1),    
                             P is Px-1, replace(B, P, B1, Bnew).
                             
/*Applies a List of moves*/
apply_moves(NewBoard, [], NewHand, NewHand, NewBoard):- !.
apply_moves(B, [[T,X,Y]|TM], Hand, NewHand, NewBoard) :- move(B, X, Y, T, Hand, NB, NewHand1), !, apply_moves(NB,TM, NewHand1, NewHand , NewBoard).

apply_moves(NewBoard, [], NewBoard) :- !.
apply_moves(B, [[T,X,Y]|TM], NewBoard) :- move(B, X, Y, T, NB), !, apply_moves(NB,TM, NewBoard).

%///////////////////////////////////////////////////////////////////////////// NEW VALID MOVE //////////////////////////////////////////////////////////////////

empty_Tile(T) :- T1 = tile(' ', ' '), T == T1.
not_empty_Tile(T, Deck) :- member(T, Deck). 

verify_Line(_B, _Limite, _Line, _Count).
verify_Line(B, Limite, Line, Count) :- Count < Limite, nth0(Count, B, Elem), empty_Tile(Elem), Count1 is Count + 1, verify_Line(B, Limite, Line, Count1).

all_Empty_Board(_B, _CC).
all_Empty_Board(B, CC) :- length(B, Altura), CC < Altura,  nth0(0, B, Elem), length(Elem, Largura), nth0(CC, B, Line),
                          verify_Line(B, Largura, Line, 0), CC1 is CC+1, all_Empty_Board(B, CC1).

verify_Left_aux(B, Px, Py, [S|LS], [C|LC]) :-  Py > 0, 
                                        getTile(B, Px, Py, T), \+ empty_Tile( T), T = tile(C, S),
                                        Py1 is Py - 1, verify_Left_aux(B, Px, Py1, LS, LC).
verify_Left_aux(B, Px, Py, LS, LC) :- (Py =:= 0;(getTile(B, Px, Py, T),empty_Tile( T ))), LS = [], LC = [], !. 

verify_Left(B, Px, Py, LS, LC):- Py1 is Py - 1, verify_Left_aux(B, Px, Py1, LS, LC).



verify_Right_aux(B, Px, Py, [S|LS], [C|LC]) :- nth0(0, B, Elem), length(Elem, Largura), Py =< Largura, 
                                        getTile(B, Px, Py, T), \+ empty_Tile( T), T = tile(C, S),
                                        Py1 is Py + 1, verify_Right_aux(B, Px, Py1, LS, LC).
verify_Right_aux(B, Px, Py, LS, LC) :- nth0(0, B, Elem), length(Elem, Largura), (Py =:= Largura+1;(getTile(B, Px, Py, T),empty_Tile( T ))), LS = [], LC = [], !. 
       
verify_Right(B, Px, Py, LS, LC):- Py1 is Py + 1, verify_Right_aux(B, Px, Py1, LS, LC).



verify_Down_aux(B, Px, Py, [S|LS], [C|LC]) :- length(B, Altura), Py =< Altura, 
                                        getTile(B, Px, Py, T), \+ empty_Tile( T), T = tile(C, S),
                                        Px1 is Px + 1, verify_Down_aux(B, Px1, Py, LS, LC).
verify_Down_aux(B, Px, Py, LS, LC) :- length(B, Altura), (Px =:= Altura+1;(getTile(B, Px, Py, T),empty_Tile( T ))), LS = [], LC = [], !. 
       
verify_Down(B, Px, Py, LS, LC):- Px1 is Px + 1, verify_Down_aux(B, Px1, Py, LS, LC).



verify_Up_aux(B, Px, Py, [S|LS], [C|LC]) :-  Px > 0, 
                                        getTile(B, Px, Py, T), \+ empty_Tile( T), T = tile(C, S),
                                        Px1 is Px - 1, verify_Up_aux(B, Px1, Py, LS, LC).
verify_Up_aux(B, Px, Py, LS, LC) :- (Px =:= 0;(getTile(B, Px, Py, T),empty_Tile( T ))), LS = [], LC = [], !. 

verify_Up(B, Px, Py, LS, LC):- Px1 is Px - 1, verify_Up_aux(B, Px1, Py, LS, LC).



inBounds(B, X, Y):- length(B, H), nth1(1, B, L), length(L, W),
                    between(1, H, X), between(1, W, Y).

isEmpty(B, X, Y):- getTile(B, X, Y, Tile2), Tile2 = tile(' ', ' ').            

verify_Vert(B, Px, Py, LS, LC) :-  verify_Up(B,  Px, Py, LS1, LC1), verify_Down(B, Px, Py, LS2, LC2), 
                                    append(LS1, LS2, LS), append(LC1, LC2, LC).  

verify_Hor(B, Px, Py, LS, LC) :- verify_Right(B, Px, Py, LS1, LC1), verify_Left(B, Px, Py, LS2, LC2),
                                 append(LS1, LS2, LS), append(LC1, LC2, LC).

valid(B, _T, _Px, _Py, Mc) :- Mc =:= 0, all_Empty_Board(B, 0), !.
valid(B, T, Px, Py, _Mc) :- inBounds(B, Px, Py), isEmpty(B, Px, Py), verify_Vert(B, Px, Py, LS, LC), 
                            T = tile(C, S), append(LC, [C], LC3), append(LS, [S], LS3),
                            !, all_same_or_different(LS3), all_same_or_different(LC3).


valid(B, T, Px, Py, _Mc) :- inBounds(B, Px, Py), isEmpty(B, Px, Py), verify_Hor(B, Px, Py, LS, LC), 
                            T = tile(C, S), append(LC, [C], LC3), append(LS, [S], LS3),
                            !, all_same_or_different(LS3), all_same_or_different(LC3).

belong_toHand(Move, Hand) :- Move = [T, _Px, _Py], member(T, Hand).

valid_ListMoves(_B, [_Move|_T], _Hand, _Mc).
valid_ListMoves(B, [Move|T], Hand, Mc) :- Move = [T, Px, Py], belong_toHand(Move, Hand), valid(B,T,Px, Py, Mc), valid_ListMoves(B, T, Hand,  Mc).



%//////////////////////////////////////////////////////////////////////////VALID-MOV/////////////////////////////////////////////////////////////////////////////

hasNeighbour(_B, _X, _Y, []):-!, fail.
hasNeighbour(B, X, Y, [[Dx, Dy]|_Ds]):- X1 is X + Dx, Y1 is Y + Dy, inBounds(B, X1, Y1), \+isEmpty(B, X1, Y1), !.
hasNeighbour(B, X, Y, [[_Dx, _Dy]|Ds]):- hasNeighbour(B, X, Y, Ds).

validPositions(_, []).
validPositions(B, [FirstMove|OtherMoves]):- FirstMove =[_Tile, X, Y], validPosition(B, X, Y), apply_moves(B, [FirstMove], NewB), validPositions(NewB, OtherMoves).



/*Verifies if a Given Tile belongs to the Hand*/

inHand(List, Hand):- sublist(Hand, List, _, _, _), List \= [].
inHandPos(List, Hand):- var(List), subl(Hand, TileTemp),TileTemp \= [],permutation(TileTemp, Tiles), extract_pos(List, Tiles, _).
inHandPos(List, Hand):- nonvar(List), extract_pos(List, Tiles, _), subl(Hand, Tiles).

/*Extracs position from a list of moves formed by Tile, Px, Py*/
extract_pos([],[],[]).
extract_pos([Move|Moves], [Tile|Tiles], [Pos|Positions]):- Move = [Tile, X, Y], Pos = [X,Y], !, extract_pos(Moves, Tiles, Positions).

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

/*Tests if a Move is valid or not, if it isnt returns a valid one...*/

validMov(B,[H|T], Hand):- validMovVert(B,[H|T], Hand, _Pont1) , validMovHor(B,[H|T], Hand, _Pont2).

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
        append(LS1, LS2, LS), 
        append(LC1, LC2, LC),
        length(LC, N),
        if((Tseen = Positions, all_same_or_different(LS), all_same_or_different(LC)),
        (if(N =:= 1, Pont is 0,
         if(N mod 5 =:= 0, Pont is N *2, Pont is N ))),Pont is 0),
        Pontf is Pont.
       

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
        if((Tseen = Positions,  all_same_or_different(LS), all_same_or_different(LC)),
        (if(N =:= 1, Pont is 0,
         if(N mod 5 =:= 0, Pont is N *2, Pont is N ))),Pont is 0),
        Pontf is Pont.

/*Tests if in a given List all elements are either all the same or all different*/
all_same_or_different(L):- all_same( L ), !.
all_same_or_different(L):- all_different( L ), !.
all_different([]).
all_different([H|T]):- \+member(H, T), all_different( T ).
all_same([]).
all_same([H|T]):- length(T,N), listElement(T, N, H).
                  
%////////////////////////////////////////////////////////////////BOT1 - RANDOM /////////////////////////////////////////////////////////////////////////


%////////////////////////////////////////////////////////////////BOT2 - SMART/////////////////////////////////////////////////////////////////////////

/*return the best Move the one that uses more tiles...*/

evaluateMov(N, Lista) :- length(Lista, N).

subl_aux(N, List, Sub):- subset(N, List, Sub).
subl_aux(N, List, Sub):- N > 1, N1 is N - 1, subl_aux(N1, List, Sub).

subl(List, Sub):- length(List, N), subl_aux(N, List, Sub).

subset(Len, [E|Tail], [E|NTail]):-   PLen is Len - 1,  (PLen > 0 -> subset(PLen, Tail, NTail) ; NTail=[]).
subset(Len, [_|Tail], NTail):-  subset(Len, Tail, NTail).

best_Mov(B, Hand, Best) :- validMov(B, Best, Hand).                                          

%/////////////////////////////////////////////////////////////////////////PLAYER1 VS PLAYER2///////////////////////////////////////////////////////////////////

/*ASKS COLER, LETTER, POSITION IN ORDER TO MOVE TILE... */
moveTile(C, S, Px, Py) :- write('Choose the tile to play. DO NOT PUT A DOT IN THE END!!!! Color: '),read_line(_),
                        read_line(X1), name(C, X1), color( C ), write('Shape: '), read_line(X2), name(S, X2), shape(S), write('Choose where to place it. Row? '),
                        read(Px), write('Column ?'), read(Py).

numberTiles(L) :- write('How many tiles do you want to play? (1,2,3,4,5)'), read(Count), Count > 0, Count < 6, movement(Count, L).

movement(0, _L).
movement(Count, [Move|L]) :- Count > 0, moveTile(C, S, Px, Py), T = tile(C, S), Move = [T, Px, Py],
                                    N1 is Count - 1, movement(N1, L).

playPlayerMove(B, Hand, NewHand, NewBoard) :- displayBoard(B), nl, displayHand(Hand,0), nl, numberTiles(L), validMov(B, L, Hand),nl, 
                                             diplay(L), apply_moves(B, L, Hand, NewHand, NewBoard),nl,diplay(L), displayBoard(NewBoard), nl, nl, diplay(L).


%/////////////////////////////////////////////////////////////////////////////MENUS/////////////////////////////////////////////////////////////////////////

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
playOp(1) :- write('\33\[2J'), nl, createBoard(3,3,B), makeDeck(Deck), mixingElemtsDeck(Deck, NewDeck),
                        creatingHand(NewDeck, Hand1, Hand2), write('          ------ Player1! -----'), game(B, 0, Hand1, Hand2, 0, 0).

playOp(2) :- write('\33\[2J'), nl, createBoard(3,3,B), makeDeck(Deck), mixingElemtsDeck(Deck, NewDeck),
                        creatingHand(NewDeck, Hand1, Hand2), write('          ------ Player1! -----'), gameHBot(B, 0, Hand1, Hand2, 0, 0).


playOp(3) :-  write('\33\[2J'), nl, createBoard(3,3,B), makeDeck(Deck), mixingElemtsDeck(Deck, NewDeck),
                        creatingHand(NewDeck, Hand1, Hand2), createCenter(B, 3, 3, Hand1, Bnew, NHand1),  write('          ------ Player1! -----'), gameBot(Bnew, 0, NHand1, Hand2, 0, 1).

playOp(4) :- menu.
playOp(5) :- abort.

/* game loop */

done([]).

playMove(B, Hand, NewHand, NewBoard) :- displayBoard(B), nl, displayHand(Hand,0), nl, numberTiles(L),valid_ListMoves(B, L, Hand, 1),nl, diplay(L), apply_moves(B, L, Hand, NewHand, NewBoard),nl,diplay(L), displayBoard(NewBoard), nl, nl, diplay(L).


playBotMov(B, Hand, NewHand, NewBoard) :- displayBoard(B), nl, displayHand(Hand,0), nl, best_Mov(B, Hand, Best),nl, 
                                  diplay(Best), apply_moves(B, Best, Hand, NewHand, NewBoard), nl, diplay(L), displayBoard(NewBoard), nl, nl, diplay(L).

game(Board,0, Hand1, Hand2, 0, 0):- \+done(Hand1), \+done(Hand2), expand_matrix_up(Board, NB), expand_matrix_down(NB, NNB), 
                                     expand_matrix_left(NNB, NNNB), expand_matrix_right(NNNB, NNNNB), playMove(NNNNB, Hand1, NewHand, NewBoard),
                                     !, write('        ------ Player2! -----'), game(NewBoard,1, NewHand, Hand2, 1, 0).

game(Board,1, Hand1, Hand2, 1, 0):- \+done(Hand1), \+done(Hand2), expand_matrix_up(Board, NB),expand_matrix_down(NB, NNB), 
                                     expand_matrix_left(NNB, NNNB), expand_matrix_right(NNNB, NNNNB) , playMove(NNNNB, Hand2, NewHand, NewBoard),
                                     !, write('        ------ Player1! -----'), game(NewBoard,0, Hand1, NewHand, 0, 0).


gameBot(Board,0, Hand1, Hand2, 0, 1):- \+done(Hand1), \+done(Hand2), expand_matrix_up(Board, NB), expand_matrix_down(NB, NNB), 
                                        expand_matrix_left(NNB, NNNB), expand_matrix_right(NNNB, NNNNB),playBotMov(NNNNB, Hand1, NewHand, NewBoard), 
                                        !, write('        ------ Player2! -----'), gameBot(NewBoard,1, NewHand, Hand2, 1, 1).
gameBot(Board,1, Hand1, Hand2, 1, 1):- \+done(Hand1), \+done(Hand2), expand_matrix_up(Board, NB), expand_matrix_down(NB, NNB), 
                                        expand_matrix_left(NNB, NNNB), expand_matrix_right(NNNB, NNNNB), playBotMov(NNNNB, Hand2, NewHand, NewBoard),
                                         !, write('        ------ Player1! -----'), gameBot(NewBoard,0, Hand1, NewHand, 0, 1).

gameHBot(Board,0, Hand1, Hand2, 0, 0):- \+done(Hand1), \+done(Hand2), expand_matrix_up(Board, NB), expand_matrix_down(NB, NNB), 
                                        expand_matrix_left(NNB, NNNB), expand_matrix_right(NNNB, NNNNB), playMove(NNNNB, Hand1, NewHand, NewBoard), 
                                        !, write('        ------ Player2! -----'), gameHBot(NewBoard,1, NewHand, Hand2, 1, 0), print('sdds').
gameHBot(Board,1, Hand1, Hand2, 1, 0):- \+done(Hand1), \+done(Hand2), expand_matrix_up(Board, NB), expand_matrix_down(NB, NNB), 
                                        expand_matrix_left(NNB, NNNB), expand_matrix_right(NNNB, NNNNB), playBotMov(NNNNB, Hand2, NewHand, NewBoard), 
                                        !, write('        ------ Player1! -----'), gameHBot(NewBoard,0, Hand1, NewHand, 0, 0).


game(_,0,_,_,_,_,_,_):- nl, write('Player 1 Won!'), nl.
game(_,1,_,_,_,_,_,_):- nl, write('Player 2 Won!'),nl.
game(_,_,_,_,_,_,0,_):- nl, write('Bot 1 Won!'), nl.
game(_,_,_,_,_,_,_,1):- nl, write('Bot 2 Won!'),nl.

/* Load librarys */
load :- use_module(library(random)), use_module(library(lists)).

/*FUNCOES DE TESTE*/

diplay([]).
diplay([H|T]) :- write(H), nl, diplay(T).

test :- createBoard(5,5, M), displayBoard(M), expand_matrix_5left(1, M, NM), displayBoard(NM).


randomBoard :- load, createBoard(5,5,B), T1 = tile(y,'!'), T2 = tile(g,'!'), T3 = tile(r,'!'), L = [[T1,2,3],[T2,3,3]], Hand = [T1,T2,T3] ,
                !, valid_ListMoves(B, L, Hand, 1), displayBoard(B), apply_moves(B, L, Hand, _NH, NB),
                L1 = [[T3,3,4]], valid_ListMoves(NB, L1, Hand, 1),apply_moves(NB, L1, Hand, _NH1, NNB), displayBoard(NNB).

t5(List) :- load, createBoard(5,5,B), T1 = tile(y,'!'),T2 = tile(g,'!'),T3 = tile(r,'!'), L = [[T1,3,4],[T2,2,4], [T3, 1,4]], Hand = [T1,T2,T3] , 
                apply_moves(B, L, Hand, NB, _NewHand), displayBoard(NB), !, valid_ListMoves(NB, List, Hand,1), ((List = [[tile(y,!),4,4],[tile(g,!),4,5],[tile(r,!),4,3]])->breakpoint; true). 



t(List) :- load, createBoard(5,5,B), T1 = tile(y,'!'),T2 = tile(g,'!'),T3 = tile(r,'!'), L = [[T1,3,4],[T2,2,4], [T3, 1,4]], Hand = [T1,T2,T3] , 
                apply_moves(B, L, Hand, NB, _NewHand), displayBoard(NB), !, validMov(NB, List, Hand), ((List = [[tile(y,!),4,4],[tile(g,!),4,5],[tile(r,!),4,3]])->breakpoint; true). 
/*
t4(Lis):- load, createBoard(5,5,B), T1 = tile(y,'!'),T2 = tile(g,'!'),T3 = tile(r,'!'), L = [[T1,3,4],[T2,2,4], [T3, 1,4]] , Hand = [T1,T2,T3],
                apply_moves(B, L, Hand, NB, _NewHand), displayBoard(NB), !, getAllValidMoves(B, Hand, Lis), diplay(Lis).
*/
/*
t3(List, Pont) :- load, createBoard(2,2,B), T1 = tile(y,'!'),T2 = tile(g,'!'),T3 = tile(r,'!'), L = [[T1,1,1],[T2,1,2]], 
                apply_moves(B, L, NB), displayBoard(NB), Hand = [T1,T3] , !, validMov(NB, List, Hand, Pont), ((List = [[tile(y,!),2,2],[tile(r,!),2,1]])->breakpoint; true).


t2(List, Pont) :- load, createBoard(5,5,B), T1 = tile(y,'!'),T2 = tile(g,'!'),T3 = tile(r,'!'), L = [[T1,3,4],[T2,2,4], [T3, 1,4]], 
                apply_moves(B, L, NB), displayBoard(NB), Hand = [T1,T2,T3] , !, validPosition(NB, 1, 1).



*/
