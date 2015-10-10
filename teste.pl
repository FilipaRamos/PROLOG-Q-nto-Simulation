cor(r). 
cor(b).
cor(g).
cor(y).

forma(*).
forma(!).
forma(#).
forma(+).
forma(&).


PlinhaHorizontal(N) :- N>0 , write(' '), write ("----"), N1 is N-1 , linhaHorizontal(N1).
linhaVertical(N) :- N>0, write('|'), write("    ") , write('|'), N1 is N-1, linhaVertical(N1).
SlinhaHorizontal(N) :-

board(N, M) :- PlinhaHorizontal(N), linhaVertical(M).