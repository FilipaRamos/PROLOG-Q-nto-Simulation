:-use_module(library(lists)).
:-use_module(library(between)).
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


/*Creating Deck*/

wildQuintoTile(C, S) :- C = c, S = s.

wildColorTile(C, _S) :- C = c.
wildShapeTile(_C, S) :- S = s. 

deckWithDuplicates(X) :- findall(T, oneTile(T), X1), findall(T, oneTile(T), X2), append(X1, X2, X).

%rmvElem(Deck, Elem, NewDeck) :- delete(Deck, Elem, NewDeck).

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


/*FUNCOES DE TESTE*/

diplay([]).
diplay([H|T]) :- write(H), nl, diplay(T).

