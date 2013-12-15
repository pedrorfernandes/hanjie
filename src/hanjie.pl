:- use_module(library(clpfd)). 
:- use_module(library(lists)).
:- use_module(library(system), [now/1]).
:- use_module(library(random), [random/3, setrand/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%          MAIN            %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve(Filename):-
        statistics(runtime, [TotalStart|_]),
        findall(Board-Time, (
                          statistics(runtime, [StartTime|_]),
                          hanjie(Filename, Board),
                          statistics(runtime, [EndTime|_]),
                          Time is EndTime - StartTime
                       ), 
                BoardsAndTimes),
        statistics(runtime, [TotalEnd|_]),
        TotalTime is TotalEnd - TotalStart,
        printSolutions(BoardsAndTimes),
        format('Search for all solutions ended in ~3d sec.~n', [TotalTime]),
        length(BoardsAndTimes, Solutions),
        print('Found '), print(Solutions), print(' solution(s).'), nl,
        fd_statistics.

hanjie(Filename, BoardRows) :-
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
        labeling([ff,up], FlatBoard).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%          MENU            %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

menu:-
        print('Welcome to Hanjie Solver - solving nonograms in a jiffy with prolog CLP and automatons galore!'), nl,
        print('1 - Help'), nl,
        print('2 - Generate a Puzzle'), nl,
        print('3 - Create Puzzle Board'), nl,
        print('4 - Create Puzzle Clues'), nl,
        print('5 - Solve a Puzzle'), nl,
        print('6 - Exit'), nl,
        getOption(Option), !,
        doOption(Option).

doOption(1):-
       printHelp,
       menu.

doOption(2):-
       generatePuzzle,
       menu.

doOption(3):-
       makePuzzleBoard,
       menu.

doOption(4):-
       makePuzzleClues,
       menu.

doOption(5):-
       solvePuzzle,
       menu.

doOption(6):-
       true.
             
generatePuzzle:-
        print('Puzzle to generate'), nl,
        print('How many rows?'), nl,
        getNumber(Rows),
        print('How many columns?'), nl,
        getNumber(Cols),
        print('Which filename to write to? (Warning: this will overwrite the file you specify!)'), nl,
        getString(Filename),
        generateRandomBoard(Rows, Cols, Filename),
        pressEnter.

makePuzzleBoard:-
        print('Please input a list of lists followed by a dot'), nl,
        print('For example: [[0,1,0],[1,0,1],[0,0,1]].'), nl,
        print('> '),
        read(BoardRows),
        skip_line,
        print('Which filename to write to? (Warning: this will overwrite the file you specify!)'), nl,
        getString(Filename),
        generateCluesToFile(BoardRows, Filename),
        pressEnter.

makePuzzleClues:-
        print('Please input a list of lists followed by a dot. These will be the clues for the rows.'), nl,
        print('For example: [[4],[1,1],[2],[1]].'), nl,
        print('> '),
        read(ClueRows),
        skip_line,
        print('Please input a list of lists followed by a dot. These will be the clues for the columns.'), nl,
        print('For example: [[3],[1,1],[1,1],[2]].'), nl,
        print('> '),
        read(ClueCols),
        skip_line,
        print('Which filename to write to? (Warning: this will overwrite the file you specify!)'), nl,
        getString(Filename),
        writeFile(Filename, ClueRows, ClueCols),
        print('File was saved with success!'), nl,
        pressEnter.

solvePuzzle:-
        print('Specify the filename of the puzzle to solve'), nl,
        getString(Filename),
        solve(Filename),
        pressEnter.
        
getNumber(Number):-
        print('> '),
        read_line(NumberCodes),
        number_codes(Number, NumberCodes).

getString(String):-
        print('> '),
        read_line(StringCodes),
        atom_codes(String, StringCodes).

pressEnter:-
        print('Press enter to continue...'), nl, print('> '),
        skip_line.

printHelp:-
        print('Hanjie is a simple puzzle game.'), nl,
        pressEnter.

getOption(Option):-
        print('> '),
        get_code(Code),
        CurrentOption is Code - 48, % '1' to 1
        skip_line,
        (
           (CurrentOption < 1 ; CurrentOption > 6) ->
                print('Invalid option! Please input a number between 1 and 6!'), nl,
                getOption(Option)
            ;
            Option is CurrentOption
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%         PRINTING         %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printSolutions([]).
printSolutions([Board-Time|BoardsAndTimes]):-
        prettyPrint(Board),
        format('Solution above found in ~3d sec.~n', [Time]), nl,
        printSolutions(BoardsAndTimes).

prettyPrint([]).
prettyPrint([Row|Board]) :-
        length(Row, Length),
        prettyFrame(Length),
        prettyPrintAux([Row|Board]), 
        prettyFrame(Length), !.

prettyFrame(FrameLength):-
        print('+-'),
        prettyFrameAux(FrameLength).

prettyFrameAux(0):- print('+'), nl.
prettyFrameAux(FrameLength):-
        FrameLength < 0 -> true;
        print('--'),
        NextLength is FrameLength - 1,
        prettyFrameAux(NextLength).

prettyPrintAux([]).
prettyPrintAux([H|T]) :-
        print('| '),
        prettyRow(H), 
        print('|'), nl,
        prettyPrintAux(T).                          

prettyRow([]).
prettyRow([H|T]):-
        prettyPiece(H),
        prettyRow(T).

prettyPiece(0) :- print('  ').
prettyPiece(1) :- print('# ').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%        FILE I/O          %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

readFile(FileName, Rows, Cols, NumberOfRows, NumberOfCols) :-
        open(FileName, read, Stream),
        readStream(Stream, Rows, Cols),
        close(Stream),
        length(Cols, NumberOfCols),
        length(Rows, NumberOfRows).

readStream(Stream, Rows, Cols) :-
        read(Stream, rows),
        read(Stream, Rows),
        read(Stream, columns),
        read(Stream, Cols)
        ;
        !, print('Invalid File!'), false.

writeFile(FileName, Rows, Cols) :-
        open(FileName, write, Stream, [if_exists(default)]),
        writeStream(Stream, Rows, Cols),
        close(Stream).

writeStream(Stream, Rows, Cols) :-
        write(Stream, rows), write(Stream, '.\n'),
        write(Stream, Rows), write(Stream, '.\n'),
        write(Stream, columns), write(Stream, '.\n'),
        write(Stream, Cols), write(Stream, '.\n').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%       CONSTRAINTS        %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
        (Sum > 0 -> automaton(Sequence, States, Arcs) ; true ), % if the row hasn't any 1's, then we already know it's filled with 0's
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

generateBoard(NumberOfRows, NumberOfCols, Board) :-
        generateList(Board, NumberOfRows),
        generateRows(Board, NumberOfCols).

generateList(List, NumberOfElements) :-
        length(List, NumberOfElements).

generateRows([], _ ).
generateRows([Row | Rows], NumberOfCols) :-
        generateList(Row, NumberOfCols),
        domain(Row, 0, 1),
        generateRows(Rows, NumberOfCols).

getPiece(Board, RowNumber, ColNumber, Piece):-
        nth1(RowNumber, Board, Row),
        nth1(ColNumber, Row, Piece).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%        AUTOMATON         %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

buildAutomaton([], _CurrentState, FinalStates, FinalArcs, FinalStates, FinalArcs).

buildAutomaton([Clue], CurrentState, CurrentStates, CurrentArcs, FinalStates, FinalArcs):-
        append(CurrentStates, [source(1), sink(LastState)], NextStates), % the states list only indicates the first and final state
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%         EXAMPLES         %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Take this brief example of restricting clues
% This automaton restricts the clues [2,2] for a sequence of any size
% Possible solution: [1, 1, 0, 1, 1]
% Or even:           [0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0]
% We must pay attention that prolog automatons don't have a final state
% So, in order to get the correct sequence, we must use sum/3 to 
% validate first before running this automaton
exampleSequence(Sequence, Size) :-
    length(Sequence, Size),
    automaton(Sequence,
        [source(1), sink(6)],
        [arc(1, 1, 2),     arc(1, 0, 1),      % as many zeros as we want in the begining
         arc(2, 1, 3),     arc(2, 0, false),  % 1's sequence in progress
         arc(3, 1, false), arc(3, 0, 4),      % 1's sequence ends, must receive 0
         arc(4, 1, 5),     arc(4, 0, 4),      % as many zeros as we want, start next 1's seq
         arc(5, 1, 6),     arc(5, 0, false),  % 1's sequence in progress
         arc(6, 1, false), arc(6, 0, 6)       % end of automaton, another 1 will fail
        ]),
    labeling([], Sequence).

% By using build automaton, we can generate exactly the same automaton as above
% If we instantiate Sequence as [2,2]
exampleBuilder(Clues, Sequence, Size):-
        buildAutomaton(Clues, 1, [], [], States, Arcs),!,
        length(Sequence, Size),
        automaton(Sequence, States, Arcs),
        labeling([], Sequence).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%      BOARD GENERATOR     %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generateRandomBoard(NumberOfRows, NumberOfCols, FileName) :-
        now(Time),
        setrand(Time),
        length(BoardRows, NumberOfRows),
        generateRandomRows(BoardRows, NumberOfCols),
        transpose(BoardRows, BoardCols),
        generateClues(BoardRows, [], ClueRows),
        generateClues(BoardCols, [], ClueCols),
        writeFile(FileName, ClueRows, ClueCols),
        prettyPrint(BoardRows),
        print('Randomized board was saved to '), print(FileName), nl.

generateRandomList(List, NumberOfElements) :-
        length(TempList, NumberOfElements),
        randomizeElements(TempList),
        validateNumberOfOnes(TempList, List).

randomizeElements([]).
randomizeElements([X|List]):-
        random(0, 100, Y),
        X is mod(Y, 2),
        randomizeElements(List).

validateNumberOfOnes([X|List], [Y|List]):-
        sumlist([X|List], OnesCounter),
        (OnesCounter > 0 ->
                Y is X
                ;
                Y is 1
        ).

generateRandomRows([], _ ).
generateRandomRows([Row | Rows], NumberOfCols) :-
        generateRandomList(Row, NumberOfCols),
        generateRandomRows(Rows, NumberOfCols).

generateClues([], FinalClues, FinalClues).
generateClues([Sequence|Board], Clues, FinalClues):-
        getClues([], Sequence, GeneratedClues),
        append(Clues, [GeneratedClues], NextClues),
        generateClues(Board, NextClues, FinalClues).

generateCluesToFile(BoardRows, Filename):-
        transpose(BoardRows, BoardCols),
        generateClues(BoardRows, [], ClueRows),
        generateClues(BoardCols, [], ClueCols),
        writeFile(Filename, ClueRows, ClueCols),
        prettyPrint(BoardRows),
        print('Puzzle was saved to '), print(Filename), nl.

% This automaton is merely a counter
% It will count the first sequence of 1's in a binary sequence
% For example: [1,1,0,1,1]
% The automaton will count 1+1+0, split the sequence to get the remainder sequence, which is [1,1]
% and it will recursively send the remainder to another automaton
% generating the clues list [2,2]
getClues(FinalClues, [], FinalClues).
getClues(Clues, Seq, FinalClues) :-
        sumlist(Seq, NumberOfOnes),
        NumberOfOnes =:= 0 ->
                getClues(Clues, [], FinalClues)
        ;
        automaton(Seq, _, Seq,
                  [source(a),sink(b),sink(c)],
                  [arc(a,0,a,[C,   I+1]),
                   arc(a,1,b,[C+1, I+1]),
                   arc(b,1,b,[C+1, I+1]),
                   arc(b,0,c,[C,   I+1]),
                   arc(c,0,c,[C,   I  ]),
                   arc(c,1,c,[C,   I  ]) ],
                  [C, I],[0, 0],[Counter, Index], []),
        append(Clues, [Counter], NewClues),
        split(Seq, Index, RestOfSeq),
        getClues(NewClues, RestOfSeq, FinalClues).

split(List, 0, List).
split([_|List], Index, Rest):-
        NextIndex is Index - 1,
        split(List, NextIndex, Rest).