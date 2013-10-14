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

nth(1,[H|_],H) :- !.
nth(X,[_|T],NTH) :- 
        NextX is X-1, 
        nth(NextX, T, NTH).

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
        get_char(_).

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
        NewRow is Row-1;
        NewRow is Row+1;
        NewColumn is Column-1;
        NewColumn is Column+1.

% TODO valid move should also check for a valid attack
validMove(Player, Row, Column, NewRow, NewColumn, Board):-
        upDownLeftOrRight(Row, Column, NewRow, NewColumn),
        empty(NewRow, NewColumn, Board).
    

userTurn(Player, Board , NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces) :- 
        print('Select position (ex: 3c, 1b..)'), nl, print('> '),
        inputPosition(Row, Column),
        (     % if
                empty(Row, Column, Board) -> 
                       dropPiece(Player, Row, Column, Board, NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces);
              % if
                isOccupiedBy(Player, Row, Column, Board) ->
                       movePiece(Player, Row, Column, Board, NewBoard);
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
        

movePiece(Player, Row, Column, Board, NewBoard):-
        print('Select position to move the piece to (ex: 1a, 5e...)'), nl, print('> '),
        inputPosition(NewRow,NewColumn),
        validMove(Player, Row, Column, NewRow, NewColumn, Board),
        removePiece(Row, Column, Board, TempBoard),
        dropPiece(Player, NewRow, NewColumn, TempBoard, NewBoard, _, _).
        

dropPiece(Player, 1, 1, [b|Tail], [Player|Tail], UnusedPieces, UnusedPieces-1).

dropPiece(Player, 1, Column, [H | TBoard], [H | TNewBoard], UnusedPieces, NewUnusedPieces) :-
        NextCol is Column-1,
        NextCol > 0,
        dropPiece(Player, 1, NextCol, TBoard, TNewBoard, UnusedPieces, NewUnusedPieces).

dropPiece(Player, Row, Column, [A,B,C,D,E | TBoard], [A,B,C,D,E | TNewBoard], UnusedPieces, NewUnusedPieces) :- 
        NextRow is Row-1,
        NextRow > 0,
        dropPiece(Player, NextRow, Column, TBoard, TNewBoard, UnusedPieces, NewUnusedPieces).
        