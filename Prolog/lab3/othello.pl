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
initBoard([[e,e,e,e,e,e],
    [e,e,e,e,e,e],
    [e,e,o,x,e,e],
    [e,e,x,o,e,e],
    [e,e,e,e,e,e],
    [e,e,e,e,e,e]],1).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 

initialize(InitialState,InitialPlyr) :-
    initBoard(InitialState, InitialPlyr).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 


% Determines the winner based on amount of white and black pieces left
winner(State, Plyr) :-
    blackCount(State, BlackCount), 
    whiteCount(State, WhiteCount),
    (
        BlackCount > WhiteCount -> Plyr = 1 ;
        BlackCount < WhiteCount -> Plyr = 2
    ).

scoreCount([], _, 0).
scoreCount([Row|Rest], Plyr, PlyrCount) :-
    rowScoreCount(Row, Plyr, RowCount),
    scoreCount(Rest, Plyr, RestCount),
    PlyrCount is RowCount + RestCount.

rowScoreCount([], _, 0).
rowScoreCount(['x'|Row], Plyr, BCount) :-
    rowScoreCount(Row, Plyr, RestCount),
    Plyr =:= 2,
    BCount is RestCount + 1.
rowScoreCount(['o'|Row], Plyr, BCount) :-
    rowScoreCount(Row, Plyr, RestCount),
    Plyr =:= 1,
    BCount is RestCount + 1.



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

% Checks both players to see if anyone have any moves left

tie(State):- \+ winner(State, _),
              moves(1,State, WMvList),
              moves(2,State, BMvList),
              length(BMvList, BN),
              BN > 0,
              length(WMvList, WN),
              WN > 0.  


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   

terminal(State) :- winner(State,_).
terminal(State) :- tie(State).

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

% sort is based on insertion sort
% it sorts it by beginning with a list of the last element
% then checks if the next element row and column index is smaller or equal to its
% if not it places it after itself on the list
% if there are more elements it will give it to the next element in the list
moves(Plyr, State, MvList) :-
    checkMoves(Plyr, State, State, 0, FndMoves),
    sortMoves(Proposed, MvList).

checkMoves(Plyr, State, [Row|Rest], RowIndex, [RowMoves|RestMoves]) :-
    NextRowIndex is RowIndex + 1,
    checkMoves(Plyr, State, Rest, NextRowIndex, RestMoves),
    checkRowMoves(Plyr, State, Row, NextRowIndex, 0, RowMoves).

checkRowMoves(Plyr, State, [Column|RestRow], RowIndex, ColumnIndex, [ValidMoves|RowMoves]) :-
    NextColumnIndex is ColumnIndex + 1,
    checkRowMoves(Plyr, RestRow, NextColumnIndex, RowMoves),
    validmove(Plyr, State, [RowIndex|ColumnIndex]).
   
sortMoves([])
sortMoves([First|Rest], Sorted) :-
    sortMoves(Rest, SortedRest),
    sort(First, SortedRest, Sorted).

sort(Move, [], [Move]).
sort([Row1, Column1], [[Row2, Column2]|Rest], [[Row1, Column1]|Sorted]) :-
    Row1 =< Row2,
    Column1 =< Column2,
    sort([Row1|Column1], Rest, Sorted).
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


nextState(Plyr, Move, State, NewState, NextPlyr) :-
    (
        Plyr =:= 1 -> NextPlyr is 2 ;
        Plyr =:= 2 -> NextPlyr is 1
    ),
    set(State, NewState, Move, Plyr),
    checkSetDirection(Plyr, Move, NewState, -1, -1, State1), % NW
    checkSetDirection(Plyr, Move, State1, -1, 0, State2), % N
    checkSetDirection(Plyr, Move, State2, -1, 1, State3), % NE
    checkSetDirection(Plyr, Move, State3, 0, -1, State4), % W
    checkSetDirection(Plyr, Move, State4, 0, 1, State5), % E
    checkSetDirection(Plyr, Move, State5, 1, -1, State6), % SW
    checkSetDirection(Plyr, Move, State6, 1, 0, State7), % S
    checkSetDirection(Plyr, Move, State7, 1, 1, State8). % SE

checkSetDirection(Plyr, [Row|Column], State, DirectionRow, DirectionColumn, NewState) :-
    NextRow is Row + DirectionRow,
    NextRow > 0,
    NextRow < 6,
    NextColumn is Column + DirectionColumn,
    NextColumn > 0,
    NextColumn < 6,
    get(State, [NextRow, NextColumn], Value),
    checkNext(Plyr, State, [NextRow|NextColumn], Value, DirectionRow, DirectionColumn, Newstate).


checkNext(Plyr, State, [NextRow|NextColumn], Value, DirectionRow, DirectionColumn, Newstate) :-
    Value =:= 1,




    %findMove(Plyr, [0, ColumnIdx], [Row|Rest], NewState) :-
    %    findColumn(Plyr, ColumnIdx, Row, NewRow).
    %    append(NewRow, Rest, NewState).
    %findMove(Plyr, [RowIdx, ColumnIdx], [Row|Rest], NewState) :-
    %    NewRowIdx is RowIdx - 1,
    %    findMove(Plyr, [NewRowIdx, ColumnIdx], Rest, Inserted),
    %    append(Row, Inserted, NewState).
    %
    %findColumn(Plyr, 0, [_|Rest], [Symbol|Rest]) :-
    %    (
    %        Plyr =:= 0 -> Symbol is 'o' ;
    %        Plyr =:= 1 -> Symbol is 'x'
    %    ).
    %findColumn(Plyr, ColumnIdx, [Idx|Rest], [Idx|NewRow]) :-
    %    NewColumnIdx is ColumnIdx - 1,
    %    findColumn(Plyr, NewColumnIdx, Rest, NewRow).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.


validmove(Plyr, State, [Row|Column]) :-
    valid_move(Plyr, State, Row, Column).

valid_move(Plyr, State, Row, Column) :-







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





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.





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
 
