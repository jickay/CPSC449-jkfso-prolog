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

