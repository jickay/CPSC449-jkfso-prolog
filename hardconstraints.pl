% Define module
:- module(hardconstraints,[]).

% Hard constraint rules


% Check for repeating forced elements, makes invalid solution

% Check for forced/forbidden conflicts, makes invalid solution

% Check for too-near invalid pairs
hasTooNear(_,[]):- false.
hasTooNear(Matches,TooNear,I):-
    nth1(I,Matches,Task),
    checkLeft(Task,Matches,TooNear) -> true ;
    checkRight(Task,Matches,TooNear) -> true ;
    J is I+1,
    hasTooNear(Matches,TooNear,J).

checkLeft(_,_,[]):- false.
checkLeft(M,Matches,[[Left,_]|TooNear]):-
    indexOf(Matches,M,MIndex),
    getLeftIndex(MIndex,Index),
    nth1(Index,Matches,MLeft),
    checkSame(MLeft,Left) -> true ; checkLeft(M,Matches,TooNear).

checkRight(_,_,[]):- false.
checkRight(M,Matches,[[_,Right]|TooNear]):-
    indexOf(Matches,M,MIndex),
    getRightIndex(MIndex,Index),
    nth1(Index,Matches,MRight),
    checkSame(MRight,Right) -> true ; checkRight(M,Matches,TooNear).

indexOf([Element|_], Element, 1):- !.
indexOf([_|Tail], Element, Index):-
    indexOf(Tail, Element, Index1), !,
    Index is Index1+1.

getLeftIndex(Index,IndexL):-
    Index > 1 -> IndexL is Index-1 ; IndexL is 8.

getRightIndex(Index,IndexR):-
        Index < 8 -> IndexR is Index+1 ; IndexR is 1.

checkSame(X,Y):- X = Y.