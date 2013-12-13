:- use_module(library(clpfd)). 
:- use_module(library(lists)).
% :- use_module(library(fdbg)).

test(Filename):-
        statistics(runtime, [T0|_]),
        hanjie(Filename),
        statistics(runtime, [T1|_]),
        T is T1 - T0,
        format('Hanjie took ~3d sec.~n', [T]).

hanjie(Filename) :-
        print('Reading from file..'), nl,
        readFile(Filename, ClueRows, ClueCols, NumberOfRows, NumberOfCols),
        generateBoard(NumberOfRows, NumberOfCols, BoardRows), !,
        transpose(BoardRows, BoardCols),
        flatten(BoardRows, FlatBoard),
        
        print('Generating row automatons..'), nl,
        generateAutomatons(ClueRows, NumberOfRows, [], [], ClueRowStates, ClueRowArcs),
        print('Generating col automatons..'), nl,
        generateAutomatons(ClueCols, NumberOfCols, [], [], ClueColStates, ClueColArcs),
        
        print('Constraining rows..'), nl,
        constrainBoard(BoardRows, ClueRows, NumberOfRows, ClueRowStates, ClueRowArcs),
        print('Constraining cols..'), nl,
        constrainBoard(BoardCols, ClueCols, NumberOfCols, ClueColStates, ClueColArcs),
        
        print('Labeling..'), nl,
        labeling([down], FlatBoard),
        prettyPrint(BoardRows).
        
prettyPrint([]).
prettyPrint([H|T]) :-
        prettyRow(H), nl,
        prettyPrint(T).

prettyRow([]).
prettyRow([H|T]):-
        prettyPiece(H),
        prettyRow(T).

prettyPiece(Piece) :-
        Piece =:= 0 ->
            print('  ')
        ;
        Piece =:= 1 ->
            print('# ').

flatten([],[]).
flatten([H|T],Vars) :-
        flatten(T,TVars),
        append(H,TVars,Vars).

constrainBoard(_Board, _ClueRows, 0, _ClueRowStates, _ClueRowArcs).
constrainBoard(Board, ClueRows, CurrentRow, ClueRowStates, ClueRowArcs) :-
        nth1(CurrentRow, Board, Sequence),
        nth1(CurrentRow, ClueRows, ClueRow),
        nth1(CurrentRow, ClueRowStates, States),
        nth1(CurrentRow, ClueRowArcs, Arcs),
        sum(ClueRow, #=, Sum),
        sum(Sequence, #=, Sum),
        automaton(Sequence, States, Arcs),
        NextRow is CurrentRow - 1,
        constrainBoard(Board, ClueRows, NextRow, ClueRowStates, ClueRowArcs).

generateAutomatons(_ClueRows, 0, FinalStates, FinalArcs, FinalStates, FinalArcs).
generateAutomatons(ClueRows, CurrentRow, CurrentStates, CurrentArcs, FinalStates, FinalArcs) :-
        nth1(CurrentRow, ClueRows, ClueRow),
        buildAutomaton(ClueRow, 1, [], [], States, Arcs), !,
        append([States], CurrentStates, NewStates),
        append([Arcs], CurrentArcs, NewArcs),
        NextRow is CurrentRow - 1,
        generateAutomatons(ClueRows, NextRow, NewStates, NewArcs, FinalStates, FinalArcs).  
        
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

buildAutomaton([], _CurrentState, FinalStates, FinalArcs, FinalStates, FinalArcs).

buildAutomaton([Clue], CurrentState, CurrentStates, CurrentArcs, FinalStates, FinalArcs):-
        append(CurrentStates, [source(1), sink(LastState)], NextStates), 
        generateArcs(Clue, CurrentState, CurrentState, [], NewArcs, _), % not connected, last sequence
        append(CurrentArcs, NewArcs, NextArcs),
        getLastState(NextArcs, LastState),
        NextState is LastState + 1,
        buildAutomaton([], NextState, NextStates, NextArcs, FinalStates, FinalArcs).

buildAutomaton([Clue|Clues], CurrentState, CurrentStates, CurrentArcs, FinalStates, FinalArcs):-
        generateArcs(Clue, CurrentState, CurrentState, [], NewArcs, connected),
        append(CurrentArcs, NewArcs, NextArcs),
        getLastState(NextArcs, LastState),
        NextState is LastState + 1,
        buildAutomaton(Clues, NextState, CurrentStates, NextArcs, FinalStates, FinalArcs).

getLastState(States, LastState):-
        last(States, Sink),
        Sink =.. [_,LastState|_].

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

% Take this brief example of restricting clues
% This automaton restricts the clues [2,2] for a sequence of any size
% Possible solution: [1, 1, 0, 1, 1]
% Or even:           [0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0]
% We must pay attention that prolog automatons don't have a final state
% So, in order to get the correct sequence, we must use sum/3 to 
% validate first before running this automaton
exampleSequence(Sequence) :-
    sum(Sequence, #=, 4),
    automaton(Sequence,
        [source(1), sink(6)],
        [arc(1, 1, 2),     arc(1, 0, 1),      % as many zeros as we want in the begining
         arc(2, 1, 3),     arc(2, 0, false),  % 1's sequence in progress
         arc(3, 1, false), arc(3, 0, 4),      % 1's sequence ends
         arc(4, 1, 5),     arc(4, 0, 4),      % as many zeros as we want, start next 1's seq
         arc(5, 1, 6),     arc(5, 0, false),  % 1's sequence in progress
         arc(6, 1, false), arc(6, 0, 6)       % end of automaton, another 1 will fail
        ]).