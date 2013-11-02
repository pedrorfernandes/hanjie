:- use_module(library(lists), [nth1/3]).
:- use_module(library(random), [random_member/2, random_permutation/2]).

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
        print('     /-------------------\\'), nl,
        print('     | a | b | c | d | e |'), nl, 
        showRow(B, 1, P1UnusedPieces, P2UnusedPieces).

showBoard(B, P1UnusedPieces, P2UnusedPieces, DropInitiative) :-
        % if
        (P1UnusedPieces =:= 0, P2UnusedPieces =:= 0) ->       % if both already used all pieces
                showBoard(B, P1UnusedPieces, P2UnusedPieces); % showing drop initiative is irrelevant
        showPieces(o, P2UnusedPieces), nl, nl,
        print('     /-------------------\\    Drop'), nl,
        print('     | a | b | c | d | e |     Initiative: '), print(DropInitiative), nl, 
        showRow(B, 1, P1UnusedPieces, P2UnusedPieces).

showRow([], 6, P1UnusedPieces, _):-
        print(' \\-----------------------/ '), nl, nl,
        showPieces(x, P1UnusedPieces), nl.

showRow([A,B,C,D,E | Tail], N, P1UnusedPieces, P2UnusedPieces) :-
        print(' |-----------------------|'), nl,
        print(' | '), print(N), print(' |'),      % row number
        printPiece(A), printPiece(B), printPiece(C), printPiece(D), printPiece(E), nl,
        N2 is N+1,
        showRow(Tail, N2, P1UnusedPieces, P2UnusedPieces).

printPiece(N):- number(N), print('   |').
printPiece(X):- print(' '), print(X), print(' |').

% UTILITY FUNCTIONS

/* % using library definition
nth1(1,[H | _], H) :- !.

nth1(X,[_ | T], Nth) :- 
        NextX is X-1, 
        nth1(NextX, T, Nth). 
*/

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

% this will return the opposing player
versus(x, o).
versus(o, x).

isBoardPosition(P):-
    member(P, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]).

% this contains 0 because there might not be enemies left, so 0 means no attack
isSecondAttack(P):-
    member(P, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]).

% converts a board position to a row and column
% TODO is there a formula that can validate these cases?
convert(5,  1, 5):- !.
convert(10, 2, 5):- !.
convert(15, 3, 5):- !.
convert(20, 4, 5):- !.
convert(25, 5, 5):- !.

convert(Position, Row, Column):-
        % got position, calculate row and column
        number(Position) ->
            Row is (Position // 5)+1,
            Column is mod(Position, 5);
        % got row and column, calculate position
        Position is Column + 5*(Row-1).

% GAME FUNCTIONS

choko:-  game([ 1, 2, 3, 4, 5,
                6, 7, 8, 9,10,
               11,12,13,14,15,
               16,17,18,19,20,
               21,22,23,24,25], x, 12, 12, x).

/*
        An Example Board that shows the
     /-------------------\  position mapping
     | a | b | c | d | e |
 |-----------------------|
 | 1 | 1 | 2 | 3 | 4 | 5 |
 |-----------------------|
 | 2 | 6 | 7 | 8 | 9 | 10|
 |-----------------------|
 | 3 | 11| 12| 13| 14| 15|
 |-----------------------|
 | 4 | 16| 17| 18| 19| 20|
 |-----------------------|
 | 5 | 21| 22| 23| 24| 25|
 \-----------------------/ 
*/

gameOver(Player, Board, PlayerUnusedPieces, OpponentUnusedPieces, Winner):-
        count(x, Board, Nx),
        count(o, Board, No),
        (
           Player == x ->
                P1UnusedPieces is PlayerUnusedPieces,
                P2UnusedPieces is OpponentUnusedPieces;
           P1UnusedPieces is OpponentUnusedPieces,
           P2UnusedPieces is PlayerUnusedPieces
        ),
        (
           Nx =:= 0, P1UnusedPieces =:= 0, Winner = o; 
           No =:= 0, P2UnusedPieces =:= 0, Winner = x
        ).

printWinner(Winner, Board):-
        showBoard(Board, 0, 0, _),
        print('****************************************'), nl, print('*  '),
        print('Player '), print(Winner), print(' is victorious! Game Over.  *'), nl,
        print('****************************************'), nl.


game(Board, x, P1UnusedPieces, P2UnusedPieces, DropInitiative) :- 
        showBoard(Board, P1UnusedPieces, P2UnusedPieces, DropInitiative), !,
                                % cut will terminate game if the next input fails
        staticval(x, Board, P1UnusedPieces, P2UnusedPieces, Value), print('value is '), print(Value), nl,
        getAllMoves(x, Board, Moves, P1UnusedPieces, DropInitiative), print(Moves), nl,
        userTurn(x, Board , NewBoard, P1UnusedPieces, NewP1UnusedPieces, DropInitiative, NewDropInitiative),
        %computerTurn(x, Board , NewBoard, P1UnusedPieces, NewP1UnusedPieces, P2UnusedPieces, DropInitiative, NewDropInitiative, hard),
        ( % if
           gameOver(x, NewBoard, NewP1UnusedPieces, P2UnusedPieces, Winner) ->
                printWinner(Winner, NewBoard);
          % else  
           game(NewBoard, o, NewP1UnusedPieces, P2UnusedPieces, NewDropInitiative)
        ).

        
game(Board, o, P1UnusedPieces, P2UnusedPieces, DropInitiative) :- 
        showBoard(Board, P1UnusedPieces, P2UnusedPieces, DropInitiative), !,
        staticval(o, Board, P2UnusedPieces, P1UnusedPieces, Value), print('value is '), print(Value), nl,
        getAllMoves(o, Board, Moves, P2UnusedPieces, DropInitiative), print(Moves), nl,
        computerTurn(o, Board , NewBoard, P2UnusedPieces, NewP2UnusedPieces, P1UnusedPieces, DropInitiative, NewDropInitiative, hard),
        ( % if
           gameOver(o, NewBoard, NewP2UnusedPieces, P1UnusedPieces, Winner) ->
                printWinner(Winner, NewBoard);
          % else 
           game(NewBoard, x, P1UnusedPieces, NewP2UnusedPieces, NewDropInitiative)
        ).

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

emptyList([]).

notEmpty(Piece):-
        Piece == x; Piece == o.

empty(Piece):-
        Piece \= x, Piece \= o.

empty(Row, Column, Board) :-
        Position is Column + 5*(Row-1),
        nth1(Position, Board, Piece),
        Piece \= x, Piece \= o.

empty(Position, Board) :-
        nth1(Position, Board, Piece),
        Piece \= x, Piece \= o.

isOccupiedBy(Piece, Position, Board):-
        nth1(Position, Board, Piece).

isOccupiedBy(Piece, Row, Column, Board):-
        Position is Column + 5*(Row-1),
        nth1(Position, Board, Piece).
        

upDownLeftOrRight(Row, Column, NewRow, NewColumn):-
        NewRow is Row-1, NewColumn is Column;
        NewRow is Row+1, NewColumn is Column;
        NewRow is Row,   NewColumn is Column-1; 
        NewRow is Row,   NewColumn is Column+1.

validDrop(Row, Column, Board):-
        empty(Row, Column, Board).

validMove(Position, NewPosition, Board):-
        convert(Position, Row, Column),
        convert(NewPosition, NewRow, NewColumn),
        validMove(Row, Column, NewRow, NewColumn, Board).

validMove(Row, Column, NewRow, NewColumn, Board):-
        upDownLeftOrRight(Row, Column, NewRow, NewColumn),
        empty(NewRow, NewColumn, Board).

validAttack(Player, Position, NewPosition, EnemyPosition, SecondEnemyPosition, Board):-
        SecondEnemyPosition \= Position, SecondEnemyPosition \= NewPosition,
        convert(Position, Row, Column),
        convert(NewPosition, NewRow, NewColumn),
        convert(SecondEnemyPosition, SecondEnemyRow, SecondEnemyColumn),
        validAttack(Player, Row, Column, NewRow, NewColumn, EnemyRow, EnemyColumn, Board),
        convert(EnemyPosition, EnemyRow, EnemyColumn),
        SecondEnemyPosition \= EnemyPosition,
        versus(Player, Enemy),
        (
                isOccupiedBy(Enemy, SecondEnemyRow, SecondEnemyColumn, Board);
                % OR
                count(Enemy, Board, 1),           % if there is only 1 enemy left
                SecondEnemyPosition =:= 0         % we accept 0 meaning no second attack is possible
        ).

validAttack(Player, Row, Column, NewRow, NewColumn, EnemyRow, EnemyColumn, Board):-
        NewRow is Row-2, NewColumn is Column,        % player moves 2 positions up
        empty(NewRow, NewColumn, Board),             % player moves to empty position
        isOccupiedBy(Enemy, Row-1, Column, Board),   % enemy is 1 position up
        Enemy \= Player, notEmpty(Enemy),            % enemy isn't player or blank
        EnemyRow is Row-1, EnemyColumn is Column;    % declares enemy position
        
        NewRow is Row+2, NewColumn is Column,
        empty(NewRow, NewColumn, Board),
        isOccupiedBy(Enemy, Row+1, Column, Board),
        Enemy \= Player, notEmpty(Enemy),
        EnemyRow is Row+1, EnemyColumn is Column;

        NewColumn is Column-2, NewRow is Row,
        empty(NewRow, NewColumn, Board),
        isOccupiedBy(Enemy, Row, Column-1, Board),
        Enemy \= Player, notEmpty(Enemy),
        EnemyRow is Row, EnemyColumn is Column-1;

        NewColumn is Column+2, NewRow is Row,
        empty(NewRow, NewColumn, Board),
        isOccupiedBy(Enemy, Row, Column+1, Board),
        Enemy \= Player, notEmpty(Enemy),
        EnemyRow is Row, EnemyColumn is Column+1.

userTurn(Player, Board , NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, DropInitiative, NewDropInitiative) :- 
        print('Select position (ex: 3c, 1b..)'), nl, print('> '),
        inputPosition(Row, Column),
        (     % if
                (empty(Row, Column, Board), PlayerUnusedPieces > 0) -> 
                       dropPiece(Player, Row, Column, Board, NewBoard),
                       PlayerNewUnusedPieces is PlayerUnusedPieces-1,
                       NewDropInitiative = DropInitiative;
              % else if
                (isOccupiedBy(Player, Row, Column, Board), DropInitiative == Player) ->
                       userMovePiece(Player, Row, Column, Board, NewBoard),
                       PlayerNewUnusedPieces is PlayerUnusedPieces,
                       versus(Player, Opponent),     % if a player moves, the drop
                       NewDropInitiative = Opponent; % initiative goes to the opponent
              % else
                print('Invalid selection!'), nl,
                userTurn(Player, Board, NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, DropInitiative, NewDropInitiative)
        ).

computerTurn(Player, Board , NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, _EnemyUnusedPieces, DropInitiative, NewDropInitiative, easy) :-
        getAllMoves(Player, Board, Moves, PlayerUnusedPieces, DropInitiative),
        random_member(RandomMove, Moves),
        print('Choosen '), print(RandomMove), nl,
        movePiece(Player, RandomMove, Board, NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, DropInitiative, NewDropInitiative).

computerTurn(Player, Board , NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, EnemyUnusedPieces, DropInitiative, NewDropInitiative, hard) :-
        minimax(Board, BestMove, _Val, 3, Player, PlayerUnusedPieces, EnemyUnusedPieces, DropInitiative),
        %alphabeta(Board, 0, 10, BestMove, _Val, 4, Player, PlayerUnusedPieces, EnemyUnusedPieces, DropInitiative),
        print('Choosen '), print(BestMove), nl,
        movePiece(Player, BestMove, Board, NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, DropInitiative, NewDropInitiative).

getAllAttacks(Player, Board, Attacks):-
         findall(Position, (isBoardPosition(Position), isOccupiedBy(Player, Position, Board)), Positions),
                    findall(Position-Attack-SecondAttack, (isBoardPosition(Attack), 
                                                            member(Position, Positions), 
                                                            isSecondAttack(SecondAttack), 
                                                            validAttack(Player, Position, Attack, _, SecondAttack, Board) 
                                                          ), Attacks).

getAllMoves(Player, Board, ShuffledMoves, PlayerUnusedPieces, DropInitiative) :-   
    (   % if
            PlayerUnusedPieces > 0 ->  % player can drop
                findall(Position, (isBoardPosition(Position), empty(Position, Board)), EmptyPositions),
                append([], EmptyPositions, Drops);
        % else  
                append([], [], Drops)
    ),
    
    (% if
            DropInitiative == Player ->  % player can move and attack
                    findall(Position, (isBoardPosition(Position), isOccupiedBy(Player, Position, Board)), Positions),
                    findall(Position-Move, (isBoardPosition(Move), member(Position, Positions), validMove(Position, Move, Board) ), Movements),
                    append(Drops, Movements, DropsAndMovements),
                    findall(Position-Attack-SecondAttack, (isBoardPosition(Attack), 
                                                            member(Position, Positions), 
                                                            isSecondAttack(SecondAttack), 
                                                            validAttack(Player, Position, Attack, _, SecondAttack, Board) 
                                                          ), Attacks),
                    append(DropsAndMovements, Attacks, Moves);
                % else    
                    append(Drops, [], Moves)
    ),
    random_permutation(Moves, ShuffledMoves). % this will add randomness to the minimax

staticval(Player, Board, PlayerUnusedPieces, EnemyUnusedPieces, Value):-
        gameOver(Player, Board, PlayerUnusedPieces, EnemyUnusedPieces, Winner),
         (
                Winner == Player,
                Value is  1000
                ;
                Value is -1000
         )
        ;
        UnusedPiecesVal is PlayerUnusedPieces - EnemyUnusedPieces,
        count(Player, Board, PlayerCount),
        versus(Player, Enemy),
        count(Enemy, Board, EnemyCount),
        PiecesVal is PlayerCount - EnemyCount,
        %getAllAttacks(Player, Board, Attacks), length(Attacks, NumberOfAttacks),
        Value is UnusedPiecesVal + PiecesVal.


minimax(Board, BestMove, Val, Depth, Player, PlayerUnusedPieces, EnemyUnusedPieces, DropInitiative) :-
  ( (Depth =:= 0 ; gameOver(Player, Board, PlayerUnusedPieces, EnemyUnusedPieces, _Winner) ) ->
    staticval(Player, Board, PlayerUnusedPieces, EnemyUnusedPieces, Val) 
    ;
    ( 
      getAllMoves(Player, Board, Moves, PlayerUnusedPieces, DropInitiative), !,
      OneDeeper is Depth - 1,
      best(Moves, BestMove, Val, OneDeeper, Board, Player, PlayerUnusedPieces, EnemyUnusedPieces, DropInitiative)
    )
  ).

best( [ Move], Move, Val, Depth, Board, Player, PlayerUnusedPieces, EnemyUnusedPieces, DropInitiative) :-
  % minimax(Board, _, Val, Depth, Player, PlayerUnusedPieces, EnemyUnusedPieces, DropInitiative),!.
  movePiece(Player, Move, Board, NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, DropInitiative, NewDropInitiative),
  versus(Player, Enemy),
  minimax(NewBoard, _, Val, Depth, Enemy, EnemyUnusedPieces, PlayerNewUnusedPieces, NewDropInitiative),!.

best([Move1|Moves], BestMove, BestVal, Depth, Board, Player, PlayerUnusedPieces, EnemyUnusedPieces, DropInitiative) :-
  movePiece(Player, Move1, Board, NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, DropInitiative, NewDropInitiative),
  versus(Player, Enemy),
  minimax(NewBoard, _, Val1, Depth, Enemy, EnemyUnusedPieces, PlayerNewUnusedPieces, NewDropInitiative),
  best(Moves, Move2, Val2, Depth, Board, Player, PlayerUnusedPieces, EnemyUnusedPieces, DropInitiative),
  betterof(Move1, Val1, Move2, Val2, BestMove, BestVal, Depth).


   %  The next predicates are used by both minimax and alphabeta


betterof(Move0, Val0, _Move1, Val1, Move0, Val0, Depth) :-
  min_to_move(Depth), Val0 > Val1, !
  ;
  max_to_move(Depth), Val0 < Val1, !.

betterof(_Move0, _Val0, Pos1, Val1, Pos1, Val1, _Depth).

%      It is max's move when the search depth is even.
max_to_move(Depth) :- 
  even(Depth).

%      It is min's move when max is not to move
min_to_move(Depth) :-
  \+ max_to_move(Depth).

odd(X) :- X mod 2 =:= 1.
even(X) :- \+ odd(X).


movePiece(Player, Position-Attack-SecondAttack, Board, NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, DropInitiative, NewDropInitiative):-
        DropInitiative == Player,
        convert(Position, Row, Column),
        convert(Attack, NewRow, NewColumn),
        convert(SecondAttack, SecondEnemyRow, SecondEnemyColumn),
        validAttack(Player, Row, Column, NewRow, NewColumn, EnemyRow, EnemyColumn, Board) ->
                removePiece(Row, Column, Board, TempBoard1),
                isOccupiedBy(Enemy, EnemyRow, EnemyColumn, Board),  % identify the enemy pieces
                removePiece(EnemyRow, EnemyColumn, TempBoard1, TempBoard2),
                dropPiece(Player, NewRow, NewColumn, TempBoard2, TempBoard3),
                % print('Enemy captured a piece!'), nl,
                (     % if
                         count(Enemy, TempBoard3, Number), Number > 0 ->
                               % showBoard(TempBoard3, 0, 0), !,
                               removePiece(SecondEnemyRow, SecondEnemyColumn, TempBoard3, NewBoard);
                      % else
                       copy(TempBoard3, NewBoard)
                ),
                PlayerNewUnusedPieces is PlayerUnusedPieces,
                versus(Player, Opponent),     % if a player moves, the drop
                NewDropInitiative = Opponent. % initiative goes to the opponent
        
movePiece(Player, Position-Move, Board, NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, DropInitiative, NewDropInitiative):-
        DropInitiative == Player,
        convert(Position, Row, Column),
        convert(Move, NewRow, NewColumn),
        validMove(Row, Column, NewRow, NewColumn, Board) ->
                removePiece(Row, Column, Board, TempBoard),
                dropPiece(Player, NewRow, NewColumn, TempBoard, NewBoard),
                PlayerNewUnusedPieces is PlayerUnusedPieces,
                versus(Player, Opponent),     % if a player moves, the drop
                NewDropInitiative = Opponent. % initiative goes to the opponent

movePiece(Player, Drop, Board, NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, DropInitiative, NewDropInitiative):-
        convert(Drop, Row, Column),
        validDrop(Row, Column, Board) ->
                dropPiece(Player, Row, Column, Board, NewBoard),
        PlayerNewUnusedPieces is PlayerUnusedPieces-1,
        NewDropInitiative = DropInitiative.

dropPiece(Player, 1, 1, [_|Tail], [Player|Tail]).

dropPiece(Player, 1, Column, [H | TBoard], [H | TNewBoard]) :-
        NextCol is Column-1,
        NextCol > 0,
        dropPiece(Player, 1, NextCol, TBoard, TNewBoard).

dropPiece(Player, Row, Column, [A,B,C,D,E | TBoard], [A,B,C,D,E | TNewBoard]) :- 
        NextRow is Row-1,
        NextRow > 0,
        dropPiece(Player, NextRow, Column, TBoard, TNewBoard).
        

removePiece(1, 1, [_|Tail], [Position|Tail], Position).

removePiece(1, Column, [H | TBoard], [H | TNewBoard], Position):-
        NextCol is Column-1,
        NewPosition is Position+1,
        NextCol > 0,
        removePiece(1, NextCol, TBoard, TNewBoard, NewPosition).
        
removePiece(Row, Column, [A,B,C,D,E | TBoard], [A,B,C,D,E | TNewBoard], Position) :-
        NextRow is Row-1,
        NewPosition is Position+5,
        NextRow > 0,
        removePiece(NextRow, Column, TBoard, TNewBoard, NewPosition).

removePiece(Row, Column, Board, NewBoard) :-
        removePiece(Row, Column, Board, NewBoard, 1).


inputSecondAttack(Enemy, Board, EnemyRow, EnemyColumn):-
        print('Select second enemy to be removed (ex: 1a, 5e...)'), nl, print('> '),
        inputPosition(Row, Column),
        isOccupiedBy(Enemy, Row, Column, Board) ->
                EnemyRow is Row, EnemyColumn is Column;
        print('Invalid enemy position!'), nl,
        inputSecondAttack(Enemy, Board, EnemyRow, EnemyColumn).          

userMovePiece(Player, Row, Column, Board, NewBoard):-
        print('Select position to move the piece to (ex: 1a, 5e...)'), nl, print('> '),
        inputPosition(NewRow,NewColumn),
        (      % if 
                validMove(Row, Column, NewRow, NewColumn, Board) ->
                        removePiece(Row, Column, Board, TempBoard),
                        dropPiece(Player, NewRow, NewColumn, TempBoard, NewBoard);
               % else if 
                validAttack(Player, Row, Column, NewRow, NewColumn, EnemyRow, EnemyColumn, Board) ->
                        removePiece(Row, Column, Board, TempBoard1),
                        isOccupiedBy(Enemy, EnemyRow, EnemyColumn, Board),  % identify the enemy pieces
                        removePiece(EnemyRow, EnemyColumn, TempBoard1, TempBoard2),
                        dropPiece(Player, NewRow, NewColumn, TempBoard2, TempBoard3),
                        print('You have captured an enemy piece!'), nl,
                        (     % if
                                count(Enemy, TempBoard3, Number), Number > 0 ->
                                        showBoard(TempBoard3, 0, 0), !,
                                        inputSecondAttack(Enemy, TempBoard3, SecondEnemyRow, SecondEnemyColumn),
                                        removePiece(SecondEnemyRow, SecondEnemyColumn, TempBoard3, NewBoard);
                              % else
                                copy(TempBoard3, NewBoard)
                        );
                                
              % else
                print('Invalid move or attack!'), nl,
                userMovePiece(Player, Row, Column, Board, NewBoard)
        ).
        