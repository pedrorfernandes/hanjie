% PRINTING FUNCTIONS

showBoard([]).

showPieces(x, 0).
showPieces(o, 0).

showPieces(Player, NumPieces):-
        print(' | '), print(Player),
        NextNumPieces is NumPieces-1,
        showPieces(Player, NextNumPieces).

showBoard(B, P1UnusedPieces, P2UnusedPieces) :-
        showPieces(o, P2UnusedPieces), nl, nl,
        print('     /-------------------\\ '), nl,
        print('     | a | b | c | d | e |'), nl, 
        showRow(B, 1, P1UnusedPieces, P2UnusedPieces).

showRow([], 6, P1UnusedPieces, _):-
        print(' \\-----------------------/ '), nl, nl,
        showPieces(x, P1UnusedPieces), nl.

showRow([A,B,C,D,E | Tail], N, P1UnusedPieces, P2UnusedPieces) :-
        print(' |-----------------------|'), nl,
        print(' | '), print(N), print('||'),      % row number
        printPiece(A), printPiece(B), printPiece(C), printPiece(D), printPiece(E), nl,
        N2 is N+1,
        showRow(Tail, N2, P1UnusedPieces, P2UnusedPieces).

printPiece(b):- print('   |').
printPiece(X):- print(' '), print(X), print(' |').

% UTILITY FUNCTIONS

nth(1,[H | _], H) :- !.

nth(X,[_ | T], Nth) :- 
        NextX is X-1, 
        nth(NextX, T, Nth).

count(_, [], 0).

count(X, [X | T], N):-
        count(X, T, N2),
        N is N2+1.

count(X, [Y | T], N):-
        X \= Y,
        count(X, T, N).

copy(L,R) :- accCp(L,R).
accCp([],[]).
accCp([H|T1],[H|T2]) :- accCp(T1,T2).
        

% GAME FUNCTIONS

choko:-  game([b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b], x, 12, 12).
         
game(Board, x, P1UnusedPieces, P2UnusedPieces) :- 
        showBoard(Board, P1UnusedPieces, P2UnusedPieces), !,
                                % cut will terminate game if the next input fails
        userTurn(x, Board , NewBoard, P1UnusedPieces, NewP1UnusedPieces),
        game(NewBoard, o, NewP1UnusedPieces, P2UnusedPieces).

game(Board, o, P1UnusedPieces, P2UnusedPieces) :- 
        showBoard(Board, P1UnusedPieces, P2UnusedPieces), !,
        userTurn(o, Board , NewBoard, P2UnusedPieces, NewP2UnusedPieces),
        game(NewBoard, x, P1UnusedPieces, NewP2UnusedPieces).

inputPosition(Row, Column):-
        getRow(Row),
        getColumn(Column),
        skip_line.

% 'a' to 1
getColumn(Column) :-
        get_code(Code),
        Column is Code - 96.

% '1' to 1
getRow(Row) :-
        get_code(Code),
        Row is Code - 48.

empty(Row, Column, Board) :-
        Position is Column + 5*(Row-1),
        nth(Position, Board, b).

isOccupiedBy(Piece, Row, Column, Board):-
        Position is Column + 5*(Row-1),
        nth(Position, Board, Piece).
        

upDownLeftOrRight(Row, Column, NewRow, NewColumn):-
        NewRow is Row-1, NewColumn is Column;
        NewRow is Row+1, NewColumn is Column;
        NewRow is Row,   NewColumn is Column-1; 
        NewRow is Row,   NewColumn is Column+1.

validMove(Row, Column, NewRow, NewColumn, Board):-
        upDownLeftOrRight(Row, Column, NewRow, NewColumn),
        empty(NewRow, NewColumn, Board).

validAttack(Player, Row, Column, NewRow, NewColumn, EnemyRow, EnemyColumn, Board):-
        NewRow is Row-2, NewColumn is Column,        % player moves 2 positions up
        empty(NewRow, NewColumn, Board),             % player moves to empty position
        isOccupiedBy(Enemy, Row-1, Column, Board),   % enemy is 1 position up
        Enemy \= Player, Enemy \= b,                 % enemy isn't player or blank
        EnemyRow is Row-1, EnemyColumn is Column;    % declares enemy position
        
        NewRow is Row+2, NewColumn is Column,
        empty(NewRow, NewColumn, Board),
        isOccupiedBy(Enemy, Row+1, Column, Board),
        Enemy \= Player, Enemy \= b,
        EnemyRow is Row+1, EnemyColumn is Column;

        NewColumn is Column-2, NewRow is Row,
        empty(NewRow, NewColumn, Board),
        isOccupiedBy(Enemy, Row, Column-1, Board),
        Enemy \= Player, Enemy \= b,
        EnemyRow is Row, EnemyColumn is Column-1;

        NewColumn is Column+2, NewRow is Row,
        empty(NewRow, NewColumn, Board),
        isOccupiedBy(Enemy, Row, Column+1, Board),
        Enemy \= Player, Enemy \= b,
        EnemyRow is Row, EnemyColumn is Column+1.

userTurn(Player, Board , NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces) :- 
        print('Select position (ex: 3c, 1b..)'), nl, print('> '),
        inputPosition(Row, Column),
        (     % if
                empty(Row, Column, Board) -> 
                       dropPiece(Player, Row, Column, Board, NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces);
              % else if
                isOccupiedBy(Player, Row, Column, Board) ->
                       movePiece(Player, Row, Column, Board, NewBoard),
                       PlayerNewUnusedPieces is PlayerUnusedPieces;
              % else
                print('Invalid selection!'), nl,
                userTurn(Player, Board, NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces)
        ).
        
        
removePiece(1, 1, [_|Tail], [b|Tail]).

removePiece(1, Column, [H | TBoard], [H | TNewBoard]):-
        NextCol is Column-1,
        NextCol > 0,
        removePiece(1, NextCol, TBoard, TNewBoard).
        
removePiece(Row, Column, [A,B,C,D,E | TBoard], [A,B,C,D,E | TNewBoard]) :-
        NextRow is Row-1,
        NextRow > 0,
        removePiece(NextRow, Column, TBoard, TNewBoard).

inputSecondAttack(Enemy, Board, EnemyRow, EnemyColumn):-
        print('Select second enemy to be removed (ex: 1a, 5e...)'), nl, print('> '),
        inputPosition(Row, Column),
        isOccupiedBy(Enemy, Row, Column, Board) ->
                EnemyRow is Row, EnemyColumn is Column;
        print('Invalid enemy position!'), nl,
        inputSecondAttack(Enemy, Board, EnemyRow, EnemyColumn).
                

movePiece(Player, Row, Column, Board, NewBoard):-
        print('Select position to move the piece to (ex: 1a, 5e...)'), nl, print('> '),
        inputPosition(NewRow,NewColumn),
        (      % if 
                validMove(Row, Column, NewRow, NewColumn, Board) ->
                        removePiece(Row, Column, Board, TempBoard),
                        dropPiece(Player, NewRow, NewColumn, TempBoard, NewBoard, _, _);
               % else if 
                validAttack(Player, Row, Column, NewRow, NewColumn, EnemyRow, EnemyColumn, Board) ->
                        removePiece(Row, Column, Board, TempBoard1),
                        isOccupiedBy(Enemy, EnemyRow, EnemyColumn, Board),  % identify the enemy pieces
                        removePiece(EnemyRow, EnemyColumn, TempBoard1, TempBoard2),
                        dropPiece(Player, NewRow, NewColumn, TempBoard2, TempBoard3, _, _),
                        (     % if
                                count(Enemy, TempBoard3, Number), Number > 0 ->
                                        inputSecondAttack(Enemy, TempBoard3, SecondEnemyRow, SecondEnemyColumn),
                                        removePiece(SecondEnemyRow, SecondEnemyColumn, TempBoard3, NewBoard);
                              % else
                                copy(TempBoard3, NewBoard)
                        );
                                
              % else
                print('Invalid move or attack!'), nl,
                movePiece(Player, Row, Column, Board, NewBoard)
        ).
        

dropPiece(Player, 1, 1, [b|Tail], [Player|Tail], UnusedPieces, UnusedPieces-1).

dropPiece(Player, 1, Column, [H | TBoard], [H | TNewBoard], UnusedPieces, NewUnusedPieces) :-
        NextCol is Column-1,
        NextCol > 0,
        dropPiece(Player, 1, NextCol, TBoard, TNewBoard, UnusedPieces, NewUnusedPieces).

dropPiece(Player, Row, Column, [A,B,C,D,E | TBoard], [A,B,C,D,E | TNewBoard], UnusedPieces, NewUnusedPieces) :- 
        NextRow is Row-1,
        NextRow > 0,
        dropPiece(Player, NextRow, Column, TBoard, TNewBoard, UnusedPieces, NewUnusedPieces).
        