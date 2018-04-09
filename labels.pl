%define module
:- module(labels,[checkLabels/1]).

%Import readLines
:- use_module(input_output,[readLines/2,printErrorAndClose/2]).

label("Name:").
label("forced partial assignment:").
label("forbidden machine:").
label("too-near tasks:").
label("machine penalties:").
label("too-near penalities").

%Takes in list of lines from input, checks if the labels are all there
checkLabels(ListOfLines):-
        label(X), is_member(X, ListOfLines).

%Recursively checks if X is a member of a list
is_member(_,[]):-
    printErrorAndClose('output.txt',"Error while parsing input file"),
    false.
is_member(X,[X|_]):- !.
is_member(X,[_|Ys]):-
    is_member(X,Ys).

split(List, Pivot, Left, Right) :- append(Left, [Pivot|Right], List).

% Partial2 = forced pairs
get_forced_partial(List, Partial2):-
    split(List, "forced partial assignment:", _, Partial1),
    split(Partial1, "forbidden machine:", Partial2, _).

% Partial2 = forbidden machines      
get_forbidden_machine(List, Partial2):-
    split(List, "forbidden machine:", _, Partial1),
    split(Partial1, "too-near tasks:", Partial2, _).
    
% Partial2 = too near tasks
get_toonear_tasks(List, Partial2):-
    split(List, "too-near tasks:", _, Partial1),
    split(Partial1, "machine penalties:", Partial2, _).

% Partial2 = machine penalties
get_machine_penalties(List, Partial2):-
    split(List, "machine penalties:", _, Partial1),
    split(Partial1, "too-near penalities", Partial2, _).

% Partial2 = too near penalties
get_toonear_penalties(List, Partial2):-
    split(List, "too-near penalities", _, Partial2).