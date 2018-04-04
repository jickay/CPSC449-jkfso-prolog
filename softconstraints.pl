% Import other modules (aka other prolog files)
% :- use_module(hardconstraints,[readLines/2]).

% Define module
:- module(softconstraints,[]).

% Sets of values
isMachine(Elem):- member(Elem, ["1","2","3","4","5","6","7","8"]).
isTask(Elem):- member(Elem,["A","B","C","D","E","F","G","H"]).

value('1',1).
value('2',2).
value('3',3).
value('4',4).
value('5',5).
value('6',6).
value('7',7).
value('8',8).

% Get all possible matches
% fillAllMatches([FM|ForcedMatches],Machs,Tasks):-
%     isMachine(Machs),
%     isTask(Tasks),

% fillAMatches([],_).
% fillAMatches([T|Tasks],[T|Matches]):-
%     fillAMatches(Tasks,Matches).
% fillAMatches([_,T|Tasks],[T|Matches]):-
%     fillAMatches(Tasks,Matches).

fillAllMatches(Tasks, AllMatches):-
    permutation(Tasks, AllMatches).

filterValidMatches([],_,_,_).
filterValidMatches([Matches|AllMTail],AllMatches,Forced,ValidMatches):-
    checkForced(Forced,Matches,Matches) ->
    filterValidMatches(AllMTail,AllMatches,Forced,ValidMatches) ;
    select(Matches,AllMatches,ValidMatches),
    filterValidMatches(AllMTail,AllMatches,Forced,ValidMatches).

checkForced([],_,_).
checkForced([[MachF,TaskF]|Forced],Matches,Matches):-
    isForced(MachF,TaskF,Matches,Matches),
    checkForced(Forced,Matches,Matches).

% isForced(_,_,[],_).
isForced(MachF,TaskF,[Task|MTail],Matches):-
    indexOf(Matches,Task,MIndex),
    value(MachF,MachInt),
    MachInt = MIndex,
    TaskF = Task;
    isForced(MachF,TaskF,MTail,Matches).

% additional functors
indexOf([Element|_], Element, 1):- !.
indexOf([_|Tail], Element, Index):-
    indexOf(Tail, Element, Index1), !,
    Index is Index1+1.