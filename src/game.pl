% PRINTING FUNCTIONS

showBoard([]).

showBoard(B) :- 
        print('    /-------------------\\'), nl,
        print('    | a | b | c | d | e |'), nl, 
        showRow(B, 1).

showRow([], 6):-
        print('\\-----------------------/'), nl.

showRow([A,B,C,D,E | Tail], N) :-
        print('|-----------------------|'), nl,
        print('| '), print(N), print('||'),      % row number
        printPiece(A), printPiece(B), printPiece(C), printPiece(D), printPiece(E), nl,
        N2 is N+1,
        showRow(Tail, N2).

printPiece(b):- print('   |').
printPiece(X):- print(' '), print(X), print(' |').


% GAME FUNCTIONS

choko:-  game([b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b], x).
         
game(Board , x) :- moveUser(Board , NewBoard), game(NewBoard, o).
game(Board , o) :- moveUser(Board , NewBoard), game(NewBoard ,x).

getPosition(Row, Column):-
        getRow(Row),
        getColumn(Column),
        get_char(_).

% 'a' to 1
getColumn(Column) :-
        get_code(Code),
        Column is Code - 96.

% '1' to 1
getRow(Row) :-
        get_code(Code),
        Row is Code - 48.

moveUser(Board , NewBoard) :- 
        showBoard(Board),!,    % cut will terminate game if the next input fails
        print('Select position (ex: 3c, 1b..)'), nl, print('> '),
        getPosition(RowNumber, ColumnNumber),
        playAt(x, RowNumber, ColumnNumber, Board, NewBoard).

playAt(Player, 1, 1, [b|Tail], [Player|Tail]).

playAt(Player, 1, Column, [H | TBoard], [H | TNewBoard]) :-
        NextCol is Column-1,
        NextCol > 0,
        playAt(Player, 1, NextCol, TBoard, TNewBoard).

playAt(Player, Row, Column, [A,B,C,D,E | TBoard], [A,B,C,D,E | TNewBoard]) :- 
        NextRow is Row-1,
        NextRow > 0,
        playAt(Player, NextRow, Column, TBoard, TNewBoard).
        