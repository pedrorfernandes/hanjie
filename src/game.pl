:- use_module(library(random), [random_member/2, random_permutation/2]).

% PRINTING FUNCTIONS

showBoard([]).

showPieces(x, 0).
showPieces(o, 0).

showPieces(Player, NumPieces):-
        print(' | '), print(Player),
        NextNumPieces is NumPieces-1,
        showPieces(Player, NextNumPieces).

showBoard(Player, Board, PlayerUnusedPieces, OpponentUnusedPieces, DropInitiative):-
        orderUnusedPieces(Player, PlayerUnusedPieces, OpponentUnusedPieces, P1UnusedPieces, P2UnusedPieces),
        showBoard(Board, P1UnusedPieces, P2UnusedPieces, DropInitiative).

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

columnToLetter(1, a).
columnToLetter(2, b).
columnToLetter(3, c).
columnToLetter(4, d).
columnToLetter(5, e).

printPosition(Position):-
        convert(Position, Row, Column),
        columnToLetter(Column, Letter),
        print(Row), print(Letter).

printMove(Drop):-
        number(Drop), nl,
        print('Computer dropped a piece in '),
        printPosition(Drop), nl, nl.

printMove(Position-Move):-
        number(Position), number(Move), nl,
        print('Computer moved '),
        printPosition(Position),
        print(' to '),
        printPosition(Move), nl, nl.

printMove(Position-Attack-SecondAttack):-
        number(Position), number(Attack), number(SecondAttack), nl,
        print('Computer attacked from '),
        printPosition(Position),
        print(' to '),
        printPosition(Attack),
        SecondAttack =\= 0 ->
                print(' and has taken '),
                printPosition(SecondAttack), nl, nl
        ;
        nl, nl.

% UTILITY FUNCTIONS

nth1(1,[H | _], H) :- !.

nth1(X,[_ | T], Nth) :- 
        NextX is X-1, 
        nth1(NextX, T, Nth). 

count(_, [], 0).

count(X, [X | T], N):-
        count(X, T, N2),
        N is N2+1.

count(X, [Y | T], N):-
        X \= Y,
        count(X, T, N).

copy(L,R) :- copyAux(L,R).
copyAux([],[]).
copyAux([H|T1],[H|T2]) :- copyAux(T1,T2).

% this will return the opposing player
versus(x, o).
versus(o, x).

isBoardPosition(P):-
    member(P, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]).

% this contains 0 because there might not be enemies left, so 0 means no attack
isSecondAttack(P):-
    member(P, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]).

% converts a board position to a row and column
% special cases that won't work with formula
convert(5,  1, 5):- !.
convert(10, 2, 5):- !.
convert(15, 3, 5):- !.
convert(20, 4, 5):- !.
convert(25, 5, 5):- !.

convert(Position, Row, Column):-
        % got position, calculate row and column
        number(Position) ->
            Row is (Position // 5)+1,
            Column is mod(Position, 5)
        ;
        % got row and column, calculate position
        Position is Column + 5*(Row-1).

printOptions:-
        print('1 - How to Play'), nl,
        print('2 - Select Player 1'), nl,
        print('3 - Select Player 2'), nl,
        print('4 - Start game'), nl.

printPlayer(PlayerType, PlayerDifficulty):-
        print(PlayerType),
        PlayerType \== human ->
        print(' '),
        print(PlayerDifficulty);
        true.
                
printHelp:-
        print('This game is easy, you can figure it out.'), nl,
        print('Press enter to continue'), nl,
        skip_line.

getOption(Option):-
        print('> '),
        get_code(Code),
        CurrentOption is Code - 48, % '1' to 1
        skip_line,
        (
           (CurrentOption < 1 ; CurrentOption > 4) ->
                print('Invalid option! Please input a number between 1 and 4!'), nl,
                getOption(Option)
            ;
            Option is CurrentOption
        ).

selectPlayer(NewPlayerType, NewPlayerDifficulty):-
        print('Please select one of the following types of players'), nl,
        print('1 - Human'), nl,
        print('2 - Computer, easy'), nl,
        print('3 - Computer, medium'), nl,
        print('4 - Computer, hard'), nl,
        getOption(Option),
        ( Option =:= 1,
          NewPlayerType = human,
          NewPlayerDifficulty = irrelevant
          ;
          Option =:= 2,
          NewPlayerType = computer,
          NewPlayerDifficulty = easy
          ;
          Option =:= 3,
          NewPlayerType = computer,
          NewPlayerDifficulty = medium
          ;
          Option =:= 4,
          NewPlayerType = computer,
          NewPlayerDifficulty = hard
        ).

% default starting players
newgame :- newgame(human, hard, computer, medium).
        
newgame(Player1Type, Player1Difficulty, Player2Type, Player2Difficulty):-
        print('Welcome to choko!'), nl,
        print('Player 1 - '), printPlayer(Player1Type, Player1Difficulty), print(' / '), 
        print('Player 2 - '), printPlayer(Player2Type, Player2Difficulty), nl,
        printOptions,
        getOption(Option),
        ( Option =:= 1,
          printHelp,
          newgame(Player1Type, Player1Difficulty, Player2Type, Player2Difficulty)
          ;
          Option =:= 2,
          selectPlayer(NewPlayer1Type, NewPlayer1Difficulty),
          newgame(NewPlayer1Type, NewPlayer1Difficulty, Player2Type, Player2Difficulty)
          ;
          Option =:= 3,
          selectPlayer(NewPlayer2Type, NewPlayer2Difficulty),
          newgame(Player1Type, Player1Difficulty, NewPlayer2Type, NewPlayer2Difficulty)
          ;
          Option =:= 4,
          choko(Player1Type, Player1Difficulty, Player2Type, Player2Difficulty)
        ).

% GAME FUNCTIONS

choko(Player1Type, Player1Difficulty, Player2Type , Player2Difficulty):-
        game([  1, 2, 3, 4, 5,
                6, 7, 8, 9,10,
               11,12,13,14,15,
               16,17,18,19,20,
               21,22,23,24,25], x, 12, 12, x, Player1Type, Player1Difficulty, Player2Type, Player2Difficulty).

choko:-  game([ 1, 2, 3, 4, 5,
                6, 7, 8, 9,10,
               11,12,13,14,15,
               16,17,18,19,20,
               21,22,23,24,25], x, 12, 12, x, computer, medium, computer, medium).

% TODO somehow fix this situation?
test:-  game([  x, o, o, o, x,
                o, 7, o, o, x,
                x, o, o, o, x,
                x, o, x, x, x,
                x, o, o, x, x], x, 0, 0, x, computer, medium, computer, medium).

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

orderUnusedPieces(Player, PlayerUnusedPieces, OpponentUnusedPieces, P1UnusedPieces, P2UnusedPieces):-
        Player == x ->
             P1UnusedPieces is PlayerUnusedPieces,
             P2UnusedPieces is OpponentUnusedPieces
        ;
        P1UnusedPieces is OpponentUnusedPieces,
        P2UnusedPieces is PlayerUnusedPieces.

gameOver(Player, Board, PlayerUnusedPieces, OpponentUnusedPieces, Winner):-
        count(x, Board, Nx),
        count(o, Board, No),
        orderUnusedPieces(Player, PlayerUnusedPieces, OpponentUnusedPieces, P1UnusedPieces, P2UnusedPieces),
        (
           Nx =:= 0, P1UnusedPieces =:= 0, Winner = o
           ; 
           No =:= 0, P2UnusedPieces =:= 0, Winner = x
        ).

printWinner(Winner, Board):-
        showBoard(Board, 0, 0, _),
        print('****************************************'), nl, print('*  '),
        print('Player '), print(Winner), print(' is victorious! Game Over.  *'), nl,
        print('****************************************'), nl.

game(Board, Player, PlayerUnusedPieces, OpponentUnusedPieces, DropInitiative, PlayerType, PlayerDifficulty, OpponentType, OpponentDifficulty) :- 
        showBoard(Player, Board, PlayerUnusedPieces, OpponentUnusedPieces, DropInitiative), !,
        playerTurn(Player, Board , NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, OpponentUnusedPieces, DropInitiative, NewDropInitiative, PlayerType, PlayerDifficulty),
        ( % if
           gameOver(Player, NewBoard, PlayerNewUnusedPieces, OpponentUnusedPieces, Winner) ->
                printWinner(Winner, NewBoard);
          % else 
           versus(Player, Opponent),
           game(NewBoard, Opponent, OpponentUnusedPieces, PlayerNewUnusedPieces, NewDropInitiative, OpponentType, OpponentDifficulty, PlayerType, PlayerDifficulty)
        ).

inputPosition(Row, Column):-
        getRow(Row),
        getColumn(Column),
        skip_line,
        ( Column > 5 -> fail; true),
        ( Column < 1 -> fail; true),
        ( Row > 5 -> fail; true),
        ( Row < 1 -> fail; true).

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

% this will ignore the second attack
validAttack(Player, Position, NewPosition, Board):-
        convert(Position, Row, Column),
        convert(NewPosition, NewRow, NewColumn),
        validAttack(Player, Row, Column, NewRow, NewColumn, _EnemyRow, _EnemyColumn, Board).

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
        EnemyRow is Row-1, EnemyColumn is Column     % declares enemy position
        ;
        NewRow is Row+2, NewColumn is Column,
        empty(NewRow, NewColumn, Board),
        isOccupiedBy(Enemy, Row+1, Column, Board),
        Enemy \= Player, notEmpty(Enemy),
        EnemyRow is Row+1, EnemyColumn is Column
        ;
        NewColumn is Column-2, NewRow is Row,
        empty(NewRow, NewColumn, Board),
        isOccupiedBy(Enemy, Row, Column-1, Board),
        Enemy \= Player, notEmpty(Enemy),
        EnemyRow is Row, EnemyColumn is Column-1
        ;
        NewColumn is Column+2, NewRow is Row,
        empty(NewRow, NewColumn, Board),
        isOccupiedBy(Enemy, Row, Column+1, Board),
        Enemy \= Player, notEmpty(Enemy),
        EnemyRow is Row, EnemyColumn is Column+1.

inputSecondAttack(Enemy, Board, EnemyRow, EnemyColumn):-
        print('Select second enemy to be removed (ex: 1a, 5e...)'), nl, print('> '),
        inputPosition(Row, Column) ->
        (      % if
                isOccupiedBy(Enemy, Row, Column, Board) ->
                        EnemyRow is Row, EnemyColumn is Column;
               % else
                print('Invalid enemy position!'), nl,
                inputSecondAttack(Enemy, Board, EnemyRow, EnemyColumn)
        );
        % input failed, retry
        print('Invalid enemy position!'), nl,
        inputSecondAttack(Enemy, Board, EnemyRow, EnemyColumn).

userMovePiece(Player, Row, Column, Board, NewBoard):-
        print('Select position to move the piece to (ex: 1a, 5e...)'), nl, print('> '),
        inputPosition(NewRow,NewColumn) ->
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
        )
        ;
        % input failed, retry
        print('Invalid move or attack!'), nl,
        userMovePiece(Player, Row, Column, Board, NewBoard).

playerTurn(Player, Board , NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, _EnemyUnusedPieces, DropInitiative, NewDropInitiative, human, _) :- 
        print('Select position (ex: 3c, 1b..)'), nl, print('> '), !,
        inputPosition(Row, Column) ->
        (     % if
                (empty(Row, Column, Board), PlayerUnusedPieces > 0) -> 
                       dropPiece(Player, Row, Column, Board, NewBoard),
                       PlayerNewUnusedPieces is PlayerUnusedPieces-1,
                       NewDropInitiative = DropInitiative;
              % else if
                (isOccupiedBy(Player, Row, Column, Board), DropInitiative == Player, 
                 userMovePiece(Player, Row, Column, Board, NewBoard) ) ->
                       PlayerNewUnusedPieces is PlayerUnusedPieces,
                       versus(Player, Opponent),     % if a player moves, the drop
                       NewDropInitiative = Opponent; % initiative goes to the opponent
              % else
                print('Invalid selection!'), nl,
                playerTurn(Player, Board, NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, _, DropInitiative, NewDropInitiative, human, _)
        )
        ;
        % input failed, retry
        print('Invalid selection!'), nl,
        playerTurn(Player, Board, NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, _, DropInitiative, NewDropInitiative, human, _).

% easy will choose a random position in the current board
playerTurn(Player, Board , NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, _EnemyUnusedPieces, DropInitiative, NewDropInitiative, computer, easy) :-
        getAllMoves(Player, Board, Moves, PlayerUnusedPieces, DropInitiative),
        random_member(RandomMove, Moves),
        printMove(RandomMove),
        movePiece(Player, RandomMove, Board, NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, DropInitiative, NewDropInitiative).

% medium will calculate the best move in the current board
playerTurn(Player, Board, NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, EnemyUnusedPieces, DropInitiative, NewDropInitiative, computer, medium) :-
        getAllMoves(Player, Board, Moves, PlayerUnusedPieces, DropInitiative),
        bestMove(Moves, BestMove, _Value, Player, Board, PlayerUnusedPieces, EnemyUnusedPieces, DropInitiative),
        printMove(BestMove),
        movePiece(Player, BestMove, Board, NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, DropInitiative, NewDropInitiative).

% hard will do a minimax, calculating the best move 3 steps ahead
playerTurn(Player, Board, NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, EnemyUnusedPieces, DropInitiative, NewDropInitiative, computer, hard) :-
        minimax(Board, BestMove, _Val, 3, Player, PlayerUnusedPieces, EnemyUnusedPieces, DropInitiative),
        printMove(BestMove),
        movePiece(Player, BestMove, Board, NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, DropInitiative, NewDropInitiative).

bestMove([Move], Move, BestValue, Player, Board, PlayerUnusedPieces, EnemyUnusedPieces, DropInitiative):-
        movePiece(Player, Move, Board, NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, DropInitiative, _NewDropInitiative),
        value(NewBoard, Player, PlayerNewUnusedPieces, EnemyUnusedPieces, BestValue).

bestMove([Move1|Moves], BestMove, BestValue, Player, Board, PlayerUnusedPieces, EnemyUnusedPieces, DropInitiative):-
        movePiece(Player, Move1, Board, NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, DropInitiative, _NewDropInitiative),
        value(NewBoard, Player, PlayerNewUnusedPieces, EnemyUnusedPieces, Value1),
        bestMove(Moves, Move2, Value2, Player, Board, PlayerUnusedPieces, EnemyUnusedPieces, DropInitiative),
        betterOf(Move1, Value1, Move2, Value2, BestMove, BestValue).
        
betterOf(Move1, Val1, _Move2, Val2, Move1, Val1) :-
  Val1 > Val2, !.

betterOf(_Move1, _Val1, Move2, Val2, Move2, Val2).

countAllAttacks(Player, Board, NumberOfAttacks):-
        findall(Position, (isBoardPosition(Position), isOccupiedBy(Player, Position, Board)), Positions),
        findall(Position-Attack, (isBoardPosition(Attack), 
                                   member(Position, Positions), 
                                   validAttack(Player, Position, Attack, Board)
                                 ), Attacks),
        length(Attacks, NumberOfAttacks).
                

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

executeAllMoves(Player, Board, Moves, NewBoardList, PlayerUnusedPieces, EnemyUnusedPieces, DropInitiative):-
        findall( NewBoard-PlayerNewUnusedPieces-EnemyUnusedPieces-NewDropInitiative,
                 ( member(Move, Moves),
                    movePiece(Player, Move, Board, NewBoard, PlayerUnusedPieces, PlayerNewUnusedPieces, DropInitiative, NewDropInitiative)
                 ),
                 NewBoardList).


value(Board, Player, PlayerUnusedPieces, EnemyUnusedPieces, Value):-
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
        countAllAttacks(Player, Board, NumberOfPlayerAttacks),
        countAllAttacks(Enemy, Board, NumberOfEnemyAttacks),
        Value is UnusedPiecesVal + PiecesVal + NumberOfPlayerAttacks - NumberOfEnemyAttacks.


minimax(Board, BestMove, Val, Depth, Player, PlayerUnusedPieces, EnemyUnusedPieces, DropInitiative) :-
  ( (Depth =:= 0 ; gameOver(Player, Board, PlayerUnusedPieces, EnemyUnusedPieces, _Winner) ) ->
    value(Board, Player, PlayerUnusedPieces, EnemyUnusedPieces, Val) 
    ;
    ( 
      getAllMoves(Player, Board, Moves, PlayerUnusedPieces, DropInitiative), !,
      OneDeeper is Depth - 1,
      best(Moves, BestMove, Val, OneDeeper, Board, Player, PlayerUnusedPieces, EnemyUnusedPieces, DropInitiative)
    )
  ).

best( [ Move], Move, Val, Depth, Board, Player, PlayerUnusedPieces, EnemyUnusedPieces, DropInitiative) :-
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
                (     % if
                         count(Enemy, TempBoard3, Number), Number > 0 ->
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

        