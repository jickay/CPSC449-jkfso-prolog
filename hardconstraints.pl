% Define module
:- module(hardconstraints,[tooNear/4]).

% Hard constraint rules


% Check for repeating forced elements, makes invalid solution

% Check for forced/forbidden conflicts, makes invalid solution

% Check for too-near invalid pairs
tooNear([],_,_).
tooNear([Matches|ListMTail],ListOfMatches,TooNear,ValidMatches):-
    % head(ListOfMatches,Matches),
    noTooNear(Matches,Matches,TooNear) ->
    tooNear(ListMTail,TooNear,ValidMatches) ;
    select(Matches,ListOfMatches,ValidMatches),
    tooNear(ListMTail,TooNear,ValidMatches).

% See if too-near violation in a set of matches
noTooNear(_,[],_).
noTooNear(Matches,[M|MTail],TooNear):-
    checkLeft(M,Matches,TooNear),
    checkRight(M,Matches,TooNear),
    noTooNear(Matches,MTail,TooNear).

checkLeft(_,_,[]).
checkLeft(M,Matches,[[Left,Right]|TooNear]):-
    checkSame(M,Right) ->
    indexOf(Matches,M,MIndex),
    getLeftIndex(MIndex,Index),
    nth1(Index,Matches,MLeft),
    checkDiff(MLeft,Left),
    checkLeft(M,Matches,TooNear) ;
    checkLeft(M,Matches,TooNear).

checkRight(_,_,[]).
checkRight(M,Matches,[[Left,Right]|TooNear]):-
    checkSame(M,Left) ->
    indexOf(Matches,M,MIndex),
    getRightIndex(MIndex,Index),
    nth1(Index,Matches,MRight),
    checkDiff(MRight,Right),
    checkRight(M,Matches,TooNear) ;
    checkRight(M,Matches,TooNear).

indexOf([Element|_], Element, 1):- !.
indexOf([_|Tail], Element, Index):-
    indexOf(Tail, Element, Index1), !,
    Index is Index1+1.

getLeftIndex(Index,IndexL):-
    Index > 1 -> IndexL is Index-1 ; IndexL is 8.

getRightIndex(Index,IndexR):-
        Index < 8 -> IndexR is Index+1 ; IndexR is 1.

checkSame(X,Y):- X == Y.
checkDiff(X,Y):- X \== Y.

%Checks if elements in a list are distinct
distinct([]).
distinct([_,[]]).
distinct([H|T]):-not(member(H,T)),distinct(T).

%Checks if two lists have distinct elements
distinct([],[]).
distinct([], _).
distinct(_, []).
distinct([H|T], T2) :-not(member(H, T2)),distinct(T, T2).
