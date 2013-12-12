:- use_module(library(clpfd)). 
:- use_module(library(lists)).
:- use_module(library(fdbg)).

go:- hanjie('flower.txt').

test(Filename):-
        statistics(runtime, [T0|_]),
        hanjie(Filename),
        statistics(runtime, [T1|_]),
        T is T1 - T0,
        format('p/0 took ~3d sec.~n', [T]).

hanjie(Filename) :-
        readFile(Filename, ClueRows, ClueCols, NumberOfRows, NumberOfCols),
        generateBoard(NumberOfRows, NumberOfCols, BoardRows), !,
        transpose(BoardRows, BoardCols),
        print('Start row automatons'), nl,
        constrainBoard(BoardRows, ClueRows, NumberOfRows),
        print('Start col automatons'), nl,
        constrainBoard(BoardCols, ClueCols, NumberOfCols),
        checkSum(BoardRows, ClueRows, NumberOfRows),
        checkSum(BoardCols, ClueCols, NumberOfCols),
        flatten(BoardRows, BoardList),
        print('start labeling'), nl,
        labeling([], BoardList),
        pretty_print(BoardRows).
        
flatten([],[]).
flatten([H|T],Vars) :-
        flatten(T,TVars),
        append(H,TVars,Vars).

checkSum(_Board, _ClueRows, 0).
checkSum(Board, ClueRows, CurrentRow) :-
        nth1(CurrentRow, Board, Row),
        nth1(CurrentRow, ClueRows, ClueRow),
        sum(ClueRow, #=, Sum),
        sum(Row, #=, Sum),
        NextRow is CurrentRow - 1,
        checkSum(Board, ClueRows, NextRow).

constrainBoard(_Board, _ClueRows, 0).
constrainBoard(Board, ClueRows, CurrentRow) :-
        nth1(CurrentRow, Board, Row),
        nth1(CurrentRow, ClueRows, ClueRow),
        constrain(Row, ClueRow),
        NextRow is CurrentRow - 1,
        checkSum(Board, ClueRows, NextRow).
                       
        
generateList(List, NumberOfElements) :-
        length(List, NumberOfElements).

generateBoard(NumberOfRows, NumberOfCols, Board) :-
        generateList(Board, NumberOfRows),
        generateRows(Board, NumberOfCols).

generateRows([], _ ).
generateRows([Row | Rows], NumberOfCols) :-
        generateList(Row, NumberOfCols),
        domain(Row, 0, 1),
        generateRows(Rows, NumberOfCols).

getRow(RowNumber, Board, Row):-
        nth1(RowNumber, Board, Row).

getCol(ColNumber, Board, Col):-
        length(Board, NumberOfRows),
        getColAux(Board, ColNumber, NumberOfRows, [], Col).

getColAux(_Board, _ColNumber, 0, Col, Col).
getColAux(Board, ColNumber, Row, CurrentCol, Col):-
        getPiece(Board, Row, ColNumber, Piece),
        append([Piece], CurrentCol, NextCol),
        NextRow is Row - 1,
        getColAux(Board, ColNumber, NextRow, NextCol, Col).

getPiece(Board, RowNumber, ColNumber, Piece):-
        nth1(RowNumber, Board, Row),
        nth1(ColNumber, Row, Piece).

readFile(FileName, Rows, Cols, NumberOfRows, NumberOfCols) :-
        open(FileName, read, Stream),
        readStream(Stream, Rows, Cols),
        close(Stream),
        length(Cols, NumberOfCols),
        length(Rows, NumberOfRows).

writeStream(Stream, Rows, Cols) :-
        write(Stream, rows),
        write(Stream, Rows),
        write(Stream, columns),
        write(Stream, Cols).

readStream(Stream, Rows, Cols) :-
        read(Stream, rows),
        read(Stream, Rows),
        read(Stream, columns),
        read(Stream, Cols)
        ;
        !, print('Invalid File!'), false.


pretty_print([]).
pretty_print([H|T]) :-
        write(H),nl,
        pretty_print(T).

constrain(Sequence, Clues):-
        buildAutomaton(Clues, 1, [source(1)], [], States, Arcs), !,
        print(States), nl,
        print(Arcs), nl,
        automaton(Sequence, States, Arcs).

buildAutomaton([], _CurrentState, FinalStates, FinalArcs, FinalStates, FinalArcs).

buildAutomaton([Clue], CurrentState, CurrentStates, CurrentArcs, FinalStates, FinalArcs):-
        generateStates(Clue, CurrentState, [], CurrentState, NewStates),
        append(CurrentStates, NewStates, NextStates), 
        generateArcs(Clue, CurrentState, CurrentState, [], NewArcs, _), % not connected, last sequence
        append(CurrentArcs, NewArcs, NextArcs),
        getLastState(NextArcs, LastState),
        NextState is LastState + 1,
        buildAutomaton([], NextState, NextStates, NextArcs, FinalStates, FinalArcs).

buildAutomaton([Clue|Clues], CurrentState, CurrentStates, CurrentArcs, FinalStates, FinalArcs):-
        generateStates(Clue, CurrentState, [], CurrentState, NewStates),
        append(CurrentStates, NewStates, NextStates), 
        generateArcs(Clue, CurrentState, CurrentState, [], NewArcs, connected),
        append(CurrentArcs, NewArcs, NextArcs),
        getLastState(NextArcs, LastState),
        NextState is LastState + 1,
        buildAutomaton(Clues, NextState, NextStates, NextArcs, FinalStates, FinalArcs).

getLastState(States, LastState):-
        last(States, Sink),
        Sink =.. [_,LastState|_].

generateStates(Clue, StartingState, CurrentStates, FinalState, FinalStates):-
        FinalState is Clue + StartingState,
        append(CurrentStates, [sink(FinalState)], FinalStates).

generateStates(Clue, StartingState, States, CurrentState, FinalStates):-
        append(States, [sink(CurrentState)], NextStates),
        NextState is CurrentState + 1,
        generateStates(Clue, StartingState, NextStates, NextState, FinalStates).

% final Arc
generateArcs(Clue, StartingState, FinalState, Arcs, FinalArcs, Connection):-
        FinalState is Clue + StartingState,
        % if a 1 appears, the sequence fails
        append(Arcs, [arc(FinalState, 1, false)], NextArcs1),
        % 1's sequence ends
        ( Connection == connected ->
                ConnectedFinalState is FinalState + 1,
                append(NextArcs1, [arc(FinalState, 0, ConnectedFinalState)], FinalArcs)
                ;
                append(NextArcs1, [arc(FinalState, 0, FinalState)], FinalArcs)
        ).

% first Arc
generateArcs(Clue, StartingState, CurrentState, Arcs, FinalArcs, Connection):-
        CurrentState is StartingState,
        % as many zeros as we want in the begining
        append(Arcs, [arc(CurrentState, 0, CurrentState)], NextArcs1),
        % 1 will begin the sequence
        NextState is CurrentState + 1,
        append(NextArcs1, [arc(CurrentState, 1, NextState)], NextArcs2),
        generateArcs(Clue, StartingState, NextState, NextArcs2, FinalArcs, Connection).
        
% middle Arcs
generateArcs(Clue, StartingState, CurrentState, Arcs, FinalArcs, Connection):-
        % if we get a 0, the sequence fails
        append(Arcs, [arc(CurrentState, 0, false)], NextArcs1),
        NextState is CurrentState + 1,
        % 1's sequence in progress
        append(NextArcs1, [arc(CurrentState, 1, NextState)], NextArcs2),
        generateArcs(Clue, StartingState, NextState, NextArcs2, FinalArcs, Connection).

% Takes this brief example of restricting clues
% This automaton restricts the clues [2,2] for a sequence of any size
% Possible solution: [1, 1, 0, 1, 1]
% Or even:           [0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0]
exampleSequence(Sequence) :-
    automaton(Sequence,
        [source(1), sink(1), sink(2), sink(3), sink(4), sink(5), sink(6)],
        [arc(1, 1, 2),     arc(1, 0, 1),      % as many zeros as we want in the begining
         arc(2, 1, 3),     arc(2, 0, false),  % 1's sequence in progress
         arc(3, 1, false), arc(3, 0, 4),      % 1's sequence ends
         arc(4, 1, 5),     arc(4, 0, 4),      % as many zeros as we want, start next 1's seq
         arc(5, 1, 6),     arc(5, 0, false),  % 1's sequence in progress
         arc(6, 1, false), arc(6, 0, 6)       % end of automaton, another 1 will fail
        ]).