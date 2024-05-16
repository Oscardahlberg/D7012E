/* shaper and maven when you log in ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: Oscar Dahlberg  
%    Student user id  : oscdah-9
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
:- ensure_loaded('play.pl').
:- ensure_loaded('testboards.pl').
%:- ensure_loaded('rndBoard.pl').
%:- ensure_loaded('stupid.pl').


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */





%

% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 


% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

%initBoard([['.','.','.','.','.','.'],
%    ['.','.','.','.','.','.'],
%    ['.','.',1,2,'.','.'],
%    ['.','.',2,1,'.','.'],
%    ['.','.','.','.','.','.'], 
%    ['.','.','.','.','.','.']],1).

initBoard([['.','.','.',1,'.','.'],
    ['.',1,'.',2,2,'.'],
    [1,2,2,2,'.','.'],
    [1,2,2,'.',2,1],
    ['.',2,'.',2,'.','.'], 
    [1,'.','.',1,1,'.']],1).

%initBoard([['.','.','.','.'],
%    ['.',1,2,'.'],
%    ['.',2,1,'.'],
%    ['.','.','.','.']],1).
% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 

initialize(InitialState, InitialPlyr) :-
    %testBoard3(InitialState).
    initBoard(InitialState, InitialPlyr).
%rndBoard(InitialState).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 

% Determines the winner based on amount of white and black pieces left
winner(State, Plyr) :-
    terminal(State),
    scoreCount(State, 1, FPlyrScore), 
    scoreCount(State, 2, SPlyrScore),
    (
        FPlyrScore < SPlyrScore -> Plyr = 1 ;
        FPlyrScore > SPlyrScore -> Plyr = 2 ;
        FPlyrScore \= SPlyrScore
    ).

scoreCount(State, Plyr, Score):-
    score(State, Plyr, Score),
    !.

score([], _, 0).
score([Column|Rest], Plyr, PlyrCount) :-
    columnScoreCount(Column, Plyr, RowCount),
    score(Rest, Plyr, RestCount),
    PlyrCount is RowCount + RestCount.

columnScoreCount([], _, 0).
columnScoreCount([Plyr|Column], Plyr, ScoreCount) :-
    columnScoreCount(Column, Plyr, RestCount),
    ScoreCount is RestCount + 1.
columnScoreCount([_|Column], Plyr, RestCount) :-
    columnScoreCount(Column, Plyr, RestCount).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

% If both moves calls return 

tie(State):- 
    terminal(State),
    scoreCount(State, 1, FPlyrScore), 
    scoreCount(State, 2, SPlyrScore),
    FPlyrScore =:= SPlyrScore.

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   

%terminal(State) :-
%    moves(1, State, MvList),
%    member('n', MvList),
%    !.

terminal(State) :-
    moves(1, State, FMvList),
    moves(2, State, SMvList),
    member('n', FMvList),
    member('n', SMvList).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%

moves(Plyr, State, CheckedList) :-
    checkMoves(Plyr, State, 0, Proposed),
    fixLists(Proposed, MvList),
    length(MvList, Size),
    checkSize(MvList, Size, CheckedList), !.
    %sortMoves(CheckedList, MvList).

checkSize(_, 0, ['n']).
checkSize(MvList, _, MvList).

checkMoves(_, _, 6, []).
checkMoves(Plyr, State, ColumnIdx, Combined) :-
    NextColumnIdx is ColumnIdx + 1,
    checkMoves(Plyr, State, NextColumnIdx, RestMoves),
    checkRowMoves(Plyr, State, [ColumnIdx, 0], ColumnMoves),
    isMove(ColumnMoves, RestMoves, Combined).

checkRowMoves(_, _, [_, 6], []).
checkRowMoves(Plyr, State, [ColumnIdx, RowIdx], Combined) :-
    NextRowIdx is RowIdx + 1,
    checkRowMoves(Plyr, State, [ColumnIdx, NextRowIdx], RestMoves),
    checkMove(Plyr, State, [ColumnIdx, RowIdx], Move),
    isMove(Move, RestMoves, Combined).

checkMove(Plyr, State, [ColumnIdx, RowIdx], [ColumnIdx, RowIdx]) :-
    validmove(Plyr, State, [ColumnIdx, RowIdx]).
checkMove(_, _, _, []).

% Helper predicates
isMove(Move, RestMoves, RestMoves) :-
    length(Move, Size),
    Size =:= 0.
isMove(Move, RestMoves, [Move|RestMoves]).

fixLists([], []).
fixLists([Moves], Moves).
fixLists([Moves|RestMoves], Fixed) :-
    fixLists(RestMoves, RestFixed),
    append(Moves, RestFixed, Fixed).

% Sorts
sortMoves([], []).
sortMoves([First|Rest], Sorted) :-
    sortMoves(Rest, SortedRest),
    sort(First, SortedRest, Sorted).

sort(Move, [], [Move]).
sort([Column1, Row1], [[Column2, Row2]|Rest], [[Column1, Row1]|Sorted]) :-
    Row1 =< Row2,
    Column1 =< Column2,
    sort([Column2, Row2], Rest, Sorted).
sort(Move1, Move2, [Move1|Move2]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%

% First finds the correct row
% then the correct column by iterativly
% decreaseing the move row & column indexes by 1
% until it they hit 0
% Then placing a x or o respectively


nextState(Plyr, Move, State, State, NextPlyr) :-
    (
        Move = ['n'] ;
        Move = n
    ),
    (
        Plyr =:= 1 -> NextPlyr is 2 ;
        Plyr =:= 2 -> NextPlyr is 1
    ).
nextState(Plyr, Move, State, NewState, NextPlyr) :-
    (
        Plyr =:= 1 -> NextPlyr is 2 ;
        Plyr =:= 2 -> NextPlyr is 1
    ),
    set(State, SetState, Move, Plyr),
    checkSet(Plyr, Move, SetState, -1, -1, State1),
    checkSet(Plyr, Move, State1, 0, -1, State2),
    checkSet(Plyr, Move, State2, 1, -1, State3),
    checkSet(Plyr, Move, State3, -1, 0, State4),
    checkSet(Plyr, Move, State4, 1, 0, State5),
    checkSet(Plyr, Move, State5, -1, 1, State6),
    checkSet(Plyr, Move, State6, 0, 1, State7),
    checkSet(Plyr, Move, State7, 1, 1, NewState).

checkSet(Plyr, Move, State, ColDir, RowDir, NewState) :-
    checkSetDir(Plyr, Move, State, ColDir, RowDir, 0, NewState).
checkSet(_, _, State, _, _, State).

% Returns true if the Column or Row is outside of the board
checkIsOutside([Column, Row], DirColumn, DirRow) :-
    NextRow is Row + DirRow,
    NextColumn is Column + DirColumn,
    (NextRow < 0 ; NextRow > 5 ; NextColumn < 0 ; NextColumn > 5).

% Increments the location the move was set and checks so that its not
% outside of the board, then gets the value and gives it to checkNext
checkSetDir(Plyr, [Column, Row], State, DirColumn, DirRow, Set, NewState) :-
    \+ checkIsOutside([Column, Row], DirColumn, DirRow),
    NextRow is Row + DirRow,
    NextColumn is Column + DirColumn,
    get(State, [NextColumn, NextRow], Value),
    checkNext(Plyr, State, [NextColumn, NextRow], Value, DirColumn, DirRow, Set, NewState).

checkNext(_, State, _, '.', _, _, _, State) :-
    fail.
checkNext(Plyr, State, _, Plyr, _, _, 0, State) :-
    fail.
checkNext(Plyr, State, _, Plyr, _, _, 1, State).
checkNext(Plyr, State, CurrCheck, Value, DirColumn, DirRow, _, NewState1) :-
    Value \= '.',
    Value \= Plyr,
    checkSetDir(Plyr, CurrCheck, State, DirColumn, DirRow, 1, NewState),
    set(NewState, NewState1, CurrCheck, Plyr).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.

validmove(Plyr, State,  Move) :-
    (
        Move = ['n'] ;
        Move = n
    ),
    moves(Plyr, State, ML),
    member(n, ML).
validmove(Plyr, State, Move) :-
    checkMoveSyn(Move),
    get(State, Move, Value),
    Value = '.',
    set(State, SetState, Move, Plyr),
    (
        checkSetDir(Plyr, Move, SetState, -1, -1, 0, _); % NW
        checkSetDir(Plyr, Move, SetState, 0, -1, 0, _); % N
        checkSetDir(Plyr, Move, SetState, 1, -1, 0, _); % NE
        checkSetDir(Plyr, Move, SetState, -1, 0, 0, _); % W
        checkSetDir(Plyr, Move, SetState, 1, 0, 0, _); % E
        checkSetDir(Plyr, Move, SetState, -1, 1, 0, _); % SW
        checkSetDir(Plyr, Move, SetState, 0, 1, 0, _); % S
        checkSetDir(Plyr, Move, SetState, 1, 1, 0, _) % SE
    ).

checkMoveSyn(Move) :-
    length(Move, 2).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.
h(State, Val) :-
    score(State, 1, FirstCount),
    score(State, 2, SndCount),
    Val is SndCount - FirstCount.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.

lowerBound(-1).



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.
upperBound(37).




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 
