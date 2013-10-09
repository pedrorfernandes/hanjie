% PRINTING FUNCTIONS

showBoard([]).

showBoard(B) :- 
        print('/-----------------------\\'), nl,
        print('|   | a | b | c | d | e |'), nl, 
        showRow(B, 1).

showRow([], 6):-
        print('\\-----------------------/'), nl.

showRow([A,B,C,D,E | Tail], N) :-
        print('|-----------------------|'), nl,
        print('| '), print(N), print(' |'),      % row number
        printPos(A), printPos(B), printPos(C), printPos(D), printPos(E), nl,
        N2 is N+1,
        showRow(Tail, N2).

printPos(b):- print('   |').
printPos(X):- print(' '), print(X), print(' |').



% GAME FUNCTIONS

choko:-  game([b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b], x).
         
 game(Board , x) :-     moveUser(Board , NewBoard), game(NewBoard, x).
%game(Board , o) :- moveComputer(Board , NewBoard), game(NewBoard , x).

moveUser(Board , NewBoard) :- 
        showBoard(Board), 
        print('Select position (ex: 3c, 1b..)'), nl, print('> '),
        get_code(Row),
        RowNum is Row - 48,         % '1' to 1
        get_code(Column),
        ColumnNum is Column - 96,   % 'a' to 1
        get_char(_),
        playAt(x, RowNum, ColumnNum, Board, NewBoard).

playAt(Player, 1, 1, [b|Tail], [Player|Tail]).

playAt(Player, 1, Column, [H | TBoard], [H | TNewBoard]) :-
        NextCol is Column-1,
        playAt(Player, 1, NextCol, TBoard, TNewBoard).


playAt(Player, Row, Column, [A,B,C,D,E | TBoard], [A,B,C,D,E | TNewBoard]) :- 
        NextRow is Row-1,
        playAt(Player, NextRow, Column, TBoard, TNewBoard).
        