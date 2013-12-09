:- use_module(library(clpfd)). 
:- use_module(library(lists)).
:- use_module(library(fdbg)).

go:- hanjie('flower.txt').

hanjie(Filename) :-
        readFile(Filename, ClueRows, ClueCols, NumberOfRows, NumberOfCols),
        generateBoard(NumberOfRows, NumberOfCols, Board),
        checkRowsSum(Board, ClueRows, NumberOfRows),
        checkColsSum(Board, ClueCols, NumberOfCols),
        flatten(Board, BoardList),
        labeling([], BoardList),
        print(Board).
        
flatten([],[]).
flatten([H|T],Vars) :-
        flatten(T,TVars),
        append(H,TVars,Vars).

checkRowsSum(_Board, _ClueRows, 0).
checkRowsSum(Board, ClueRows, CurrentRow) :-
        getRow(CurrentRow, Board, Row),
        getRow(CurrentRow, ClueRows, ClueRow),
        sum(ClueRow, #=, Sum),
        sum(Row, #=, Sum),
        NextRow is CurrentRow - 1,
        checkRowsSum(Board, ClueRows, NextRow).

checkColsSum(_Board, _ClueRows, 0).
checkColsSum(Board, ClueCols, CurrentCol) :-
        getCol(CurrentCol, Board, Col),
        getRow(CurrentCol, ClueCols, ClueCol),
        sum(ClueCol, #=, Sum),
        sum(Col, #=, Sum),
        NextCol is CurrentCol - 1,
        checkColsSum(Board, ClueCols, NextCol).
        
generateList(List, NumberOfElements) :-
        functor(Aux, list, NumberOfElements),
        Aux =.. ListAux,
        trimFirstElement(ListAux, List).

trimFirstElement([_|Board], Board).

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

readStream(Stream, Rows, Cols) :-
        read(Stream, rows),
        read(Stream, Rows),
        read(Stream, columns),
        read(Stream, Cols)
        ;
        !, print('Invalid File!'), false.

