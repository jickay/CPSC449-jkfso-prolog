% Define module
:- module(hardconstraints,[]).

% Hard constraint rules


% Check for repeating forced elements, makes invalid solution

% Check for forced/forbidden conflicts, makes invalid solution

% Check for too-near invalid pairs
tooNear(_,[]).
tooNear(Matches,TooNear):-
    hasTooNear(M,TooNear)

hasTooNear(M,[Left,Right|TooNear]):-
    M = Left,

